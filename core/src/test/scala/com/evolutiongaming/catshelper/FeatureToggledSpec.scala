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
import scala.concurrent.duration._

class FeatureToggledSpec extends AnyFreeSpec {
  implicit val ioRuntime: IORuntime = IORuntime.global

  "end-to-end polling" in scope { scope =>
    import scope._, env._

    val pollInterval = 10.seconds
    for {
      flag <- Ref[IO].of(false)
      ftr = FeatureToggled.polling(baseResource, flag.get, pollInterval)

      _ <- ftr.use { access =>
        def expect(
          fetchResult: Option[Int],
          expectedEvents: List[Int],
        )(implicit
          pos: Position,
        ): IO[Unit] = {
          // Polling events first to make sure they are independent from access
          (events, access.use(IO.pure)).tupled.map(_ shouldBe ((expectedEvents, fetchResult))).void
        }

        for {
          startTime <- getTime

          // We started in "off" state so there nothing after the first poll.
          _ <- sleepUntil(startTime + 1.nano)
          _ <- expect(None, List())

          // We're "on" but the poll is yet to come in 1 ns.
          _ <- flag.set(true)
          _ <- sleepUntil(startTime + pollInterval - 1.nano)
          _ <- expect(None, List())

          // And after the poll we're up.
          _ <- sleepUntil(startTime + pollInterval + 1.nano)
          _ <- expect(Some(1), List(1))

          // Still up after a few polls.
          _ <- sleepUntil(startTime + pollInterval * 2 - 1.nano)
          _ <- expect(Some(1), List(1))

          // Toggling off must release the resource on the next poll.
          _ <- flag.set(false)
          _ <- sleepUntil(startTime + pollInterval * 2 + 1.nano)
          _ <- expect(None, List(1, -1))

          // And up again, with a brand new resource.
          _ <- flag.set(true)
          _ <- sleepUntil(startTime + pollInterval * 3 + 1.nano)
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

    def localScope(body: LocalScope => IO[Unit]): Unit = scope { scope =>
      import scope._, env._

      for {
        toggle <- std.Queue.bounded[IO, Boolean](1).flatTap(_.offer(true))
        ftr = FeatureToggled.of(baseResource, gracePeriod)(toggle.take.flatMap(_).foreverM)

        _ <- ftr.use { access =>
          IO.sleep(1.nano) *> body.apply((scope, access, toggle.offer(_)))
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

    "keeps resource alive while in use" in localScope {
      case (scope, access, toggle) =>
        import scope._, env._

        val targetTime = 1.second
        for {
          holder <- access.use(_ => sleepUntil(targetTime) *> events).start

          _ <- IO.sleep(1.nano)
          _ <- toggle(false)

          // Resource must become immediately unavailable for new access.
          _ <- IO.sleep(1.nano)
          _ <- access.use(IO.pure).timeout(1.nano).map(_ shouldBe None)

          // But must be still alive while it's in use.
          _ <- sleepUntil(targetTime - 1.nano)
          _ <- events.map(_ shouldBe List(1))
          _ <- holder.join.flatMap {
            case Succeeded(value) => value.map(_ shouldBe List(1))
            case other => fail(s"Expected outcome Succeeded but was $other")
          }

          // Finally it goes down as soon as there is no usages.
          _ <- sleepUntil(targetTime + 1.nano)
          _ <- events.map(_ shouldBe List(1, -1))
        } yield ()
    }

    "terminate resource in-use after grace period" in localScope {
      case (scope, access, toggle) =>
        import scope._, env._

        for {
          holder <- access.use(_ => sleepUntil(gracePeriod + 1.minute)).start

          _ <- IO.sleep(1.nano)
          _ <- toggle(false)

          // Resource in-use stays alive during grace period.
          toggledOffAt <- getTime
          _ <- sleepUntil(toggledOffAt + gracePeriod - 1.nano)
          _ <- events.map(_ shouldBe List(1))

          // And gets forcefully terminated after.
          _ <- sleepUntil(toggledOffAt + gracePeriod + 1.nano)
          _ <- events.map(_ shouldBe List(1, -1))

          // A forcefully terminated client must still be able to finish its work.
          _ <- holder.join.flatMap {
            case Succeeded(_) => IO.unit
            case other => fail(s"Expected outcome Succeeded but was $other")
          }
        } yield ()
    }

    "release of the outer resource waits for the users, but no longer than the grace period" in scope { scope =>
      import scope._, env._

      for {
        toggleRef <- Deferred[IO, Boolean => IO[Unit]]
        ftr = FeatureToggled.of(baseResource, gracePeriod)(toggle => toggleRef.complete(toggle) *> IO.never)
        letGo <- Deferred[IO, Unit]

        startedAt <- getTime
        // The client outlives the outer resource on purpose: it is the only way to observe how
        // long the release waits for the clients that are still around.
        holder <- ftr.use { access =>
          for {
            toggle <- toggleRef.get
            _ <- toggle(true)
            _ <- IO.sleep(1.nano)
            holder <- access.use(_ => letGo.get).start
            _ <- IO.sleep(1.nano)
          } yield holder
        }
        releasedAt <- getTime

        // The resource is gone, and waiting for it took the whole grace period.
        _ <- events.map(_ shouldBe List(1, -1))
        _ = (releasedAt - startedAt) should be >= gracePeriod

        _ <- letGo.complete(())
        _ <- holder.joinWithNever
      } yield ()
    }

    "expose the same resource again when the toggle goes back on while draining" in pendingUntilFixed {
      manualScope {
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
  }

  "failure handling" - {
    "keep trying after the base resource fails to acquire" in pendingUntilFixed {
      ioTest { env =>
        import env._

        for {
          attempts <- Ref[IO].of(0)
          // The first attempt to bring the resource up fails, the following ones succeed.
          resource = Resource.eval(attempts.updateAndGet(_ + 1)).flatMap {
            case 1 => Resource.eval(IO.raiseError[Int](new RuntimeException("cannot acquire")))
            case attempt => Resource.pure[IO, Int](attempt)
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
    }

    "keep polling after a failed read of the flag" in pendingUntilFixed {
      ioTest { env =>
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
  }

  "race-conditions" - {
    "don't get stuck after multiple concurrent uses" in {
      val scenario = for {
        seed <- Ref[IO].of(1)
        flag <- Ref[IO].of(true)

        _ <- FeatureToggled.polling(seed.get.toResource, flag.get, 1.milli).use { access =>
          // Polling instead of sleeping for a fixed time: a loaded CI machine can need more than
          // a few milliseconds to catch up, and that must not fail the test.
          def awaitValue(expected: Option[Int]): IO[Unit] = {
            access.use(IO.pure).flatMap {
              case value if value == expected => IO.unit
              case _ => IO.sleep(1.milli) *> awaitValue(expected)
            }
          }

          for {
            _ <- {
              val useOnce = access.use(_ => IO.cede)
              val sequentialUses = List.fill(1000)(useOnce).sequence_
              List.fill(8)(sequentialUses).parSequence_
            }

            _ <- flag.set(false)
            _ <- awaitValue(None)

            _ <- seed.set(2)
            _ <- flag.set(true)
            _ <- awaitValue(Some(2))
          } yield ()
        }
      } yield ()

      // `unsafeRunTimed` reports a hang as `None` instead of failing, so the result must be checked.
      scenario.unsafeRunTimed(10.seconds) shouldBe Some(())
    }

    "never hand out a resource that is already being released" in pendingUntilFixed {
      val rounds = 10000
      val clientsPerRound = 4

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
          val clients = List.fill(clientsPerRound)(user).parSequence_

          toggleRef.get.flatMap { toggle =>
            /* Clients and a toggle-off racing each other, over and over again. Real threads are
             * required here: a simulated scheduler always registers the client before it runs the
             * toggle-off, so the gap this test aims at never opens. */
            List.fill(rounds)(toggle(true) *> (clients, toggle(false)).parTupled.void).sequence_
          }
        }
      } yield ()

      scenario.unsafeRunTimed(10.seconds) shouldBe Some(())
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
          val init = counter.modify(count => (count + 1, count + 1)).flatTap(count => events.update(_ enqueue count))
          init.map(count => count -> events.update(_ enqueue -count))
        }
        _ <- body(Scope(env, resource, events.get.map(_.toList)))
      } yield ()
    }
  }

  private def getTime(
    implicit
    testRuntime: TestRuntime[IO],
  ) = testRuntime.getTimeSinceStart

  private def sleepUntil(
    deadline: FiniteDuration,
  )(implicit
    testRuntime: TestRuntime[IO],
  ) = testRuntime.sleepUntil(deadline)
}
