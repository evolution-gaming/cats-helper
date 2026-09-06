package com.evolutiongaming.catshelper

import cats.effect.std.Semaphore
import cats.effect.{IO, Resource}
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
 * `ToTry[IO]` is how `skafka` bridges Kafka's rebalance callback: the IO runs to completion on the
 * poll thread, bounded by `ioToTry(1.minute)`.
 *
 * `kafka-flow` runs partition recovery through that bridge: resource-shaped work guarded by a
 * semaphore permit inside `.uncancelable`. The previous `ioToTry` stepped through `.uncancelable`
 * and `onCancel` before handing the remainder to `unsafeRunTimed`, which cancelled it on timeout
 * without waiting: the permit was never released and every subsequent poll blocked on the
 * semaphore. The consumer silently stopped processing.
 *
 * All three specs fail against the previous `ioToTry` and pass with the cooperative timeout.
 */
class ToTryTimeoutSpec extends AnyFunSuite with Matchers {

  private val defaultTimeout = 200.millis

  // longer than any test waits; a passing run never reaches it
  private val slowRecovery = 10.seconds

  test("timeout releases a resource acquired before the async boundary") {
    val f = fixture()

    f.toTry(f.recovery) should matchPattern { case Failure(_: TimeoutException) => }

    f.recorded shouldEqual Vector("acquired", "released")
  }

  test("a timed-out attempt does not strand the guard it acquired") {
    val f = fixture()

    f.toTry(f.recovery) should matchPattern { case Failure(_: TimeoutException) => }

    // what a retry around the flow does next
    f.toTry(f.recovery) should matchPattern { case Failure(_: TimeoutException) => }

    f.recorded shouldEqual Vector("acquired", "released", "acquired", "released")
  }

  test("an uncancelable guarded recovery completes and releases its guard") {
    val f = fixture(timeout = 100.millis, recoveryDuration = 300.millis)

    // `TopicFlow` guards `add`, `apply` and its own release with one permit, taken inside
    // `uncancelable`: a permit lost to cancellation would block all three forever
    f.toTry(f.guardedRecovery) shouldEqual Success(())
    f.toTry(f.guardedRecovery) shouldEqual Success(())

    f.recorded shouldEqual Vector("acquired", "released", "acquired", "released")
  }

  private def fixture(
    timeout: FiniteDuration = defaultTimeout,
    recoveryDuration: FiniteDuration = slowRecovery,
  ): Fixture =
    new Fixture(
      toTry = ToTry.ioToTry(timeout),
      guard = Semaphore[IO](1).unsafeRunSync(),
      log = new AtomicReference(Vector.empty[String]),
      recoveryDuration = recoveryDuration,
    )

  /**
   * `guard` stands in for `TopicFlow`'s semaphore: the permit that must survive a timeout.
   * `recoveryDuration` is how long the recovery body runs past the async boundary.
   */
  private class Fixture(
    val toTry: ToTry[IO],
    guard: Semaphore[IO],
    log: AtomicReference[Vector[String]],
    recoveryDuration: FiniteDuration,
  ) {

    def recorded: Vector[String] = log.get()

    /**
     * Mimics `PartitionFlow`: take the guard and finish the acquire synchronously, then rebuild
     * state across an asynchronous boundary. The release gives the guard back.
     */
    def recovery: IO[Unit] =
      Resource
        .make(guard.acquire *> record("acquired"))(_ => record("released") *> guard.release)
        .use(_ => IO.sleep(recoveryDuration))

    // wrapped the way `TopicFlow` wraps it, so the permit cannot be lost to cancellation
    def guardedRecovery: IO[Unit] = recovery.uncancelable

    private def record(event: String): IO[Unit] = IO(log.updateAndGet(_ :+ event)).void
  }
}
