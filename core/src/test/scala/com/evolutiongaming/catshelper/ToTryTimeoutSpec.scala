package com.evolutiongaming.catshelper

import cats.effect.std.Semaphore
import cats.effect.{IO, Resource}
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
 * `skafka` runs a Kafka rebalance callback through `ToTry[IO]`, so the effect has to finish on the
 * poll thread within the ambient `ioToTry(1.minute)`. `kafka-flow` recovers a partition inside such
 * a callback and holds a semaphore permit for the whole recovery, as
 * `semaphore.permit.use { ... }.uncancelable`, so that a half-done recovery cannot leave the flow
 * inconsistent.
 *
 * The previous `ioToTry` stepped inside that `uncancelable` and past the finalizers, then cancelled
 * what was left when the timeout fired. The permit was never given back, so every later call on the
 * flow blocked on the semaphore and the consumer stopped processing without failing anything
 * (kafka-flow#937). Every test below fails against that implementation.
 */
class ToTryTimeoutSpec extends AnyFunSuite with Matchers {

  // longer than `ToTry` waits; a passing run never reaches it
  private val slowRecovery = 1.minute

  test("a timeout releases what the effect acquired, and gives the permit back") {
    val guard = Semaphore[IO](1).unsafeRunSync()
    val released = new AtomicBoolean(false)
    // the permit is taken on the calling thread, the state rebuild sleeps past the timeout
    val recovery = guard.permit.use { _ =>
      Resource.onFinalize(IO(released.set(true))).use(_ => IO.sleep(slowRecovery))
    }

    ToTry.ioToTry(50.millis).apply(recovery) should matchPattern { case Failure(_: TimeoutException) => }

    released.get() shouldBe true
    guard.available.unsafeRunSync() shouldEqual 1L
  }

  test("a retry after a timed-out attempt can take the permit again") {
    val guard = Semaphore[IO](1).unsafeRunSync()
    val toTry = ToTry.ioToTry(50.millis)
    val recovery = guard.permit.use(_ => IO.sleep(slowRecovery))

    toTry(recovery) should matchPattern { case Failure(_: TimeoutException) => }
    // what a retry around the flow does next
    toTry(recovery) should matchPattern { case Failure(_: TimeoutException) => }

    guard.available.unsafeRunSync() shouldEqual 1L
  }

  test("an uncancelable guarded recovery runs past the timeout and gives the permit back") {
    val guard = Semaphore[IO](1).unsafeRunSync()
    val cancelled = new AtomicBoolean(false)
    // `kafka-flow` guards `add`, `apply` and its own release with one permit, taken inside
    // `uncancelable`: a permit lost to cancellation would block all three forever
    val recovery = guard
      .permit
      .use(_ => IO.sleep(200.millis).onCancel(IO(cancelled.set(true))))
      .uncancelable

    ToTry.ioToTry(50.millis).apply(recovery) shouldEqual Success(())

    cancelled.get() shouldBe false
    guard.available.unsafeRunSync() shouldEqual 1L
  }

  test("nested masks: a timeout reaches a polled region and gives the permit back") {
    val guard = Semaphore[IO](1).unsafeRunSync()
    val cancelled = new AtomicBoolean(false)
    // the sleep sits under two `uncancelable`, both polled, so it stays cancelable. The step stops
    // at the outermost one and hands the whole block over, nesting included; `IO.timeout` then
    // follows cats-effect's ordinary rules for it
    val recovery = guard.permit.use { _ =>
      IO.uncancelable { outer =>
        outer(IO.uncancelable(inner => inner(IO.sleep(slowRecovery).onCancel(IO(cancelled.set(true))))))
      }
    }

    ToTry.ioToTry(50.millis).apply(recovery) should matchPattern { case Failure(_: TimeoutException) => }

    cancelled.get() shouldBe true
    guard.available.unsafeRunSync() shouldEqual 1L
  }
}
