package com.evolutiongaming.catshelper

import cats.effect.implicits._
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, std}
import cats.implicits._
import com.evolutiongaming.catshelper.testkit.PureTest.ioTest
import com.evolutiongaming.catshelper.testkit.{PureTest, TestRuntime}
import org.scalactic.source.Position
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class FeatureToggledSpec extends AnyFreeSpec {
  implicit val ioRuntime: IORuntime = IORuntime.global

  "end-to-end polling" in scope { s =>
    import s._, env._

    val d = 10.seconds
    for {
      flag <- Ref[IO].of(false)
      ftr = FeatureToggled.polling(baseResource, flag.get, d)

      _ <- ftr.use { access =>
        def expect(
          fetchResult: Option[Int],
          es: List[Int],
        )(implicit
          pos: Position,
        ): IO[Unit] = {
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
        toggle <- std.Queue.bounded[IO, Boolean](1).flatTap(_.offer(true))
        ftr = FeatureToggled.of(baseResource, gracePeriod)(toggle.take.flatMap(_).foreverM)

        _ <- ftr.use { access =>
          IO.sleep(1.nano) *> f((s, access, toggle.offer(_)))
        }
      } yield ()
    }

    /* Same as `localScope`, but hands the toggle function itself over to a test, so that the
     * toggle can be flipped with no scheduling in between. */
    def manualScope(body: LocalScope => IO[Unit]): Unit = scope { scope =>
      import scope._, env._

      for {
        toggleRef <- Deferred[IO, Boolean => IO[Unit]]
        ftr = FeatureToggled.of(baseResource, gracePeriod)(toggle => toggleRef.complete(toggle) *> IO.never)

        _ <- ftr.use { access =>
          for {
            toggle <- toggleRef.get
            _ <- toggle(true)
            _ <- IO.sleep(1.nano)
            _ <- body.apply((scope, access, toggle))
          } yield ()
        }
      } yield ()
    }

    "keeps resource alive while in use" in localScope { ls =>
      val (s, access, toggle) = ls
      import s._, env._

      val targetTime = 1.second
      for {
        f1 <- access.use(_ => sleepUntil(targetTime) *> events).start

        _ <- IO.sleep(1.nano)
        _ <- toggle(false)

        // Resource must become immediately unavailable for new access.
        _ <- IO.sleep(1.nano)
        _ <- access.use(IO.pure).timeout(1.nano).map(_ shouldBe None)

        // But must be still alive while it's in use.
        _ <- sleepUntil(targetTime - 1.nano)
        _ <- events.map(_ shouldBe List(1))
        _ <- f1.join.flatMap {
          case Succeeded(value) => value.map(_ shouldBe List(1))
          case x => fail(s"Expected outcome Succeeded but was $x")
        }

        // Finally it goes down as soon as there is no usages.
        _ <- sleepUntil(targetTime + 1.nano)
        _ <- events.map(_ shouldBe List(1, -1))
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

    "expose the same resource again when the toggle goes back on while draining" ignore manualScope {
      case (scope, access, toggle) =>
        import scope._, env._

        for {
          // Keeps the resource in use, so that the toggle-off starts draining instead of
          // releasing the resource right away.
          holder <- access.use(_ => sleepUntil(10.seconds)).start

          _ <- IO.sleep(1.nano)
          _ <- toggle(false)
          _ <- IO.sleep(1.second)
          _ <- toggle(true)
          _ <- IO.sleep(1.nano)

          // The resource was never released, so it must be available again.
          _ <- access.use(IO.pure).map(_ shouldBe Some(1))
          _ <- events.map(_ shouldBe List(1))

          _ <- holder.joinWithNever
        } yield ()
    }
  }

  "failure handling" - {
    "keep trying after the base resource fails to acquire" ignore ioTest { env =>
      import env._

      for {
        attempts <- Ref[IO].of(0)
        // The first attempt to bring the resource up fails, the following ones succeed.
        resource = Resource.eval(attempts.updateAndGet(_ + 1)).flatMap {
          case 1 => Resource.eval(IO.raiseError[Int](new RuntimeException("cannot acquire")))
          case n => Resource.pure[IO, Int](n)
        }
        flag <- Ref[IO].of(true)

        _ <- FeatureToggled.polling(resource, flag.get, 1.second).use { access =>
          for {
            // The very first poll fails to bring the resource up.
            _ <- IO.sleep(1.nano)
            _ <- access.use(IO.pure).map(_ shouldBe None)

            // The next poll must try again.
            _ <- IO.sleep(1.second)
            _ <- access.use(IO.pure).map(_ shouldBe Some(2))
          } yield ()
        }
      } yield ()
    }

    "keep polling after a failed read of the flag" ignore ioTest { env =>
      import env._

      for {
        reads <- Ref[IO].of(0)
        // The first read of the flag fails, the following ones report "on".
        enabled = reads.updateAndGet(_ + 1).flatMap {
          case 1 => IO.raiseError[Boolean](new RuntimeException("cannot read the flag"))
          case _ => IO.pure(true)
        }

        _ <- FeatureToggled.polling(Resource.pure[IO, Int](1), enabled, 1.second).use { access =>
          for {
            // Nothing to see yet: the only poll so far has failed.
            _ <- IO.sleep(1.nano)
            _ <- access.use(IO.pure).map(_ shouldBe None)

            // A failed poll must not stop the polling.
            _ <- IO.sleep(1.second)
            _ <- access.use(IO.pure).map(_ shouldBe Some(1))
          } yield ()
        }
      } yield ()
    }
  }

  "race-conditions" - {
    final class Env(
      implicit
      val ec: ExecutionContext,
    )
    val env = cats.effect.Resource {
      IO {
        val tp = java.util.concurrent.Executors.newFixedThreadPool(32)
        val ec = scala.concurrent.ExecutionContext.fromExecutor(tp)
        val env = new Env()(ec)
        env -> IO { tp.shutdown() }
      }
    }

    "don't get stuck after multiple concurrent uses" in {
      env
        .use { _ =>
          for {
            seed <- Ref[IO].of(1)
            flag <- Ref[IO].of(true)
            _ <- FeatureToggled.polling(seed.get.toResource, flag.get, 1.milli).use { access =>
              for {
                _ <- {
                  val one = access.use(_ => IO.cede)
                  val loop = List.fill(1000)(one).sequence_
                  List.fill(8)(loop).parSequence_
                }
                _ <- flag.set(false)
                _ <- IO.sleep(100.millis)
                _ <- seed.set(2)
                _ <- flag.set(true)
                _ <- IO.sleep(10.millis)
                _ <- access.use(i => IO { i shouldBe Some(2) })
              } yield ()
            }
          } yield ()
        }
        .unsafeRunTimed(10.seconds)
    }

    "never hand out a resource that is already being released" ignore {
      val rounds = 10000

      val scenario = for {
        toggleRef <- Deferred[IO, Boolean => IO[Unit]]
        // The resource reports whether it is still alive.
        resource = Resource.make(Ref[IO].of(true))(_.set(false))
        ftr = FeatureToggled.of(resource, 1.minute)(toggle => toggleRef.complete(toggle) *> IO.never)

        _ <- ftr.use { access =>
          // A client that got the resource must be able to use it: the grace period is far from
          // being over, so nobody is allowed to release it in the meantime.
          val user = access.use {
            case Some(alive) => IO.cede *> alive.get.map(_ shouldBe true)
            case None => IO.unit
          }

          toggleRef.get.flatMap { toggle =>
            // A client and a toggle-off racing each other, over and over again.
            List.fill(rounds)(toggle(true) *> (user, toggle(false)).parTupled.void).sequence_
          }
        }
      } yield ()

      scenario.unsafeRunTimed(10.seconds)
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
        events <- Ref[IO].of(Queue.empty[Int])
        resource = Resource[IO, Int] {
          val init = counter.modify(i => (i + 1, i + 1)).flatTap(i => events.update(_ enqueue i))
          init.map(i => i -> events.update(_ enqueue -i))
        }
        _ <- body(Scope(env, resource, events.get.map(_.toList)))
      } yield ()
    }
  }

  private def getTime(
    implicit
    rt: TestRuntime[IO],
  ) = rt.getTimeSinceStart

  private def sleepUntil(
    dt: FiniteDuration,
  )(implicit
    rt: TestRuntime[IO],
  ) = rt.sleepUntil(dt)
}
