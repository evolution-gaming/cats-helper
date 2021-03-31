package com.evolutiongaming.catshelper

import cats.effect.concurrent.MVar
import cats.effect.{IO, Resource}
import cats.implicits._
import com.evolutiongaming.catshelper.testkit.PureTest.ioTest
import com.evolutiongaming.catshelper.testkit.{PureTest, TestRuntime}
import org.scalactic.source.Position
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import cats.effect.{ Ref, Temporal }

class FeatureToggledSpec extends AnyFreeSpec {
  "end-to-end polling" in scope { s =>
    import s._, env._

    val d = 10.seconds
    for {
      flag <- Ref[IO].of(false)
      ftr   = FeatureToggled.polling(baseResource, flag.get, d)

      _ <- ftr.use { access =>
        def expect(fetchResult: Option[Int], es: List[Int])(implicit pos: Position): IO[Unit] = {
          // Polling events first to make sure they are independent from access
          (events, access.use(IO.pure)).tupled.map(_ shouldBe ((es, fetchResult))).void
        }

        for {
          t0 <- getTime

          // We started in "off" state so there nothing after the first poll.
          _ <- sleepUntil(t0 + 1.nano)
          _ <- expect(None, List())

          // We're "on" but the poll is yet to come in 1 ns.
          _ <- flag.set(true)
          _ <- sleepUntil(t0 + d - 1.nano)
          _ <- expect(None, List())

          // And after the poll we're up.
          _ <- sleepUntil(t0 + d + 1.nano)
          _ <- expect(Some(1), List(1))

          // Still up after a few polls.
          _ <- sleepUntil(t0 + d + d - 1.nano)
          _ <- expect(Some(1), List(1))

          // Going down.
          _ <- flag.set(false)
          _ <- sleepUntil(t0 + d + d + 1.nano)
          _ <- expect(None, List(1, -1))

          // And up again.
          _ <- flag.set(true)
          _ <- sleepUntil(t0 + d + d + d + 1.nano)
          _ <- expect(Some(2), List(1, -1, 2))
        } yield ()
      }

      // Make sure we have cleaned up when we're out of `use` block.
      _ <- events.map(_ shouldBe List(1, -1, 2, -2))
    } yield ()
  }

  "graceful shutdown" - {
    val gracePeriod = 1.minute
    type LocalScope = (Scope, Resource[IO, Option[Int]], Boolean => IO[Unit])

    def localScope(f: LocalScope => IO[Unit]): Unit = scope { s =>
      import s._, env._

      for {
        toggle <- MVar[IO].of(true)
        ftr     = FeatureToggled.of(baseResource, gracePeriod)(toggle.take.flatMap(_).foreverM)

        _ <- ftr.use { access =>
          IO.sleep(1.nano) *> f((s, access, toggle.put(_)))
        }
      } yield ()
    }


    "keeps resource alive while in use" in localScope { ls =>
      val (s, access, toggle) = ls
      import s._, env._

      val targetTime = 1.second
      for {
        f1 <- access.use(_ => sleepUntil(targetTime) *> events).start

        _  <- IO.sleep(1.nano)
        _  <- toggle(false)

        // Resource must become immediately unavailable for new access.
        _  <- IO.sleep(1.nano)
        _  <- access.use(IO.pure).timeout(1.nano).map(_ shouldBe None)

        // But must be still alive while it's in use.
        _  <- sleepUntil(targetTime - 1.nano)
        _  <- events.map(_ shouldBe List(1))
        _  <- f1.join.map(_ shouldBe List(1))

        // Finally it goes down as soon as there is no usages.
        _  <- sleepUntil(targetTime + 1.nano)
        _  <- events.map(_ shouldBe List(1, -1))
      } yield ()
    }

    "terminate resource in-use after grace period" in localScope { ls =>
      val (s, access, toggle) = ls
      import s._, env._

      for {
        _ <- access.use(_ => sleepUntil(gracePeriod + 1.minute)).start

        _ <- IO.sleep(1.nano)
        _ <- toggle(false)

        // Resource in-use stays alive during grace period.
        t <- getTime
        _ <- sleepUntil(t + gracePeriod - 1.nano)
        _ <- events.map(_ shouldBe List(1))

        // And gets forcefully terminated after.
        _ <- sleepUntil(t + gracePeriod + 1.nano)
        _ <- events.map(_ shouldBe List(1, -1))
      } yield ()
    }
  }

  "race-conditions" - {
    final class Env(implicit val ec: ExecutionContext, val cs: ContextShift[IO], val timer: Temporal[IO])
    val env = cats.effect.Resource {
      IO {
        val tp = java.util.concurrent.Executors.newFixedThreadPool(32)
        val ec = scala.concurrent.ExecutionContext.fromExecutor(tp)
        val env = new Env()(ec, IO.contextShift(ec), IO.timer(ec))
        env -> IO { tp.shutdown() }
      }
    }

    "don't get stuck after multiple concurrent uses" in {
      env
        .use { env =>
          import env._

          for {
            seed <- Ref[IO].of(1)
            flag <- Ref[IO].of(true)
            r <- FeatureToggled.polling(Resource.eval(seed.get), flag.get, 1.milli).allocated.map(_._1)
            _ <- {
              val one = r.use(_ => IO.shift)
              val loop = List.fill(1000)(one).sequence_
              List.fill(8)(loop).parSequence_
            }
            _ <- flag.set(false)
            _ <- IO.sleep(100.millis)
            _ <- seed.set(2)
            _ <- flag.set(true)
            _ <- IO.sleep(10.millis)
            _ <- r.use(i => IO { i shouldBe Some(2) })
          } yield ()
        }
        .unsafeRunTimed(10.seconds)
    }
  }

  private case class Scope(
    env: PureTest.Env[IO],
    baseResource: Resource[IO, Int],
    events: IO[List[Int]],
  )

  private def scope(body: Scope => IO[Unit]): Unit = {
    ioTest { env =>
      import env._
      for {
        counter <- Ref[IO].of(0)
        events  <- Ref[IO].of(Queue.empty[Int])
        resource = Resource[IO, Int] {
          val init = counter.modify(i => (i + 1, i + 1)).flatTap(i => events.update(_ enqueue i))
          init.map(i => i -> events.update(_ enqueue -i))
        }
        _ <- body(Scope(env, resource, events.get.map(_.toList)))
      } yield ()
    }
  }

  private def getTime(implicit rt: TestRuntime[IO]) = rt.getTimeSinceStart

  private def sleepUntil(dt: FiniteDuration)(implicit rt: TestRuntime[IO]) = rt.sleepUntil(dt)
}
