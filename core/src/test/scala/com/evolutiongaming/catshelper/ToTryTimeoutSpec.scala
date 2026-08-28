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
 * `kafka-flow` runs partition recovery through that bridge — resource-shaped work guarded by a
 * semaphore permit inside `.uncancelable`. Before #330, `unsafeRunTimed` cancelled the fiber on
 * timeout, bypassing `.uncancelable`: the permit was never released and every subsequent poll
 * blocked on the semaphore. The consumer silently stopped processing.
 *
 * All three specs fail without the #330 fix and pass with it.
 */
class ToTryTimeoutSpec extends AnyFunSuite with Matchers {

  private val defaultTimeout = 200.millis

  /**
   * Longer than any test is willing to wait: a passing run never reaches it.
   */
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

    // `TopicFlow` guards `add`, `apply` and the flow's own release with a single permit, and takes
    // it inside `uncancelable` for exactly this reason: a permit lost to cancellation would block
    // all three forever.
    f.toTry(f.guardedRecovery) shouldEqual Success(())
    f.toTry(f.guardedRecovery) shouldEqual Success(())

    f.recorded shouldEqual Vector("acquired", "released", "acquired", "released")
  }

  private def fixture(
    timeout: FiniteDuration = defaultTimeout,
    releaseDuration: FiniteDuration = Duration.Zero,
    recoveryDuration: FiniteDuration = slowRecovery,
  ): Fixture =
    new Fixture(
      toTry = ToTry.ioToTry(timeout),
      guard = Semaphore[IO](1).unsafeRunSync(),
      log = new AtomicReference(Vector.empty[String]),
      releaseDuration = releaseDuration,
      recoveryDuration = recoveryDuration,
    )

  /**
   * @param toTry
   *   the bridge under test, built with the timeout under test
   * @param guard
   *   stands in for `TopicFlow`'s semaphore: the permit that must survive a timeout
   * @param log
   *   collects the "acquired"/"released" events the specs assert on
   * @param releaseDuration
   *   how long the resource release takes
   * @param recoveryDuration
   *   how long the recovery body runs past the async boundary
   */
  private class Fixture(
    val toTry: ToTry[IO],
    guard: Semaphore[IO],
    log: AtomicReference[Vector[String]],
    releaseDuration: FiniteDuration,
    recoveryDuration: FiniteDuration,
  ) {

    /**
     * The events `record`ed so far, in order.
     */
    def recorded: Vector[String] = log.get()

    /**
     * Mimics `PartitionFlow`'s acquisition: take the guard and finish the acquire synchronously,
     * then rebuild state across an asynchronous boundary. The release gives the guard back.
     */
    def recovery: IO[Unit] =
      Resource
        .make(guard.acquire *> record("acquired")) { _ =>
          IO.sleep(releaseDuration) *> record("released") *> guard.release
        }
        .use(_ => IO.sleep(recoveryDuration))

    /**
     * The `recovery`, but wrapped in `uncancelable` — the same way as `TopicFlow` wraps it, so that
     * the permit cannot be lost to cancellation.
     */
    def guardedRecovery: IO[Unit] = recovery.uncancelable

    /**
     * Crude logging: appends an event for [[recorded]] to read back.
     */
    private def record(event: String): IO[Unit] = IO(log.updateAndGet(_ :+ event)).void
  }
}
