package com.evolutiongaming.catshelper

import cats.effect.std.Semaphore
import cats.effect.{IO, Ref, Resource}
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
 * `skafka` runs a Kafka rebalance callback through `ToTry[IO]`, and `kafka-flow` recovers a
 * partition inside one, holding a semaphore permit across the recovery as
 * `semaphore.permit.use { ... }.uncancelable`.
 *
 * These tests pin what a timeout does to that shape: finalizers run, the permit comes back, and a
 * masked recovery is left to finish. The previous `ioToTry` gave none of the three and left the
 * flow blocked on its own semaphore (kafka-flow#937), so every test below fails against it.
 *
 * The conversion blocks the thread it is called on, so each test hands it to `IO.blocking`, as
 * `skafka` does on the poll thread. Its own timing is real: a clock outside it would not govern the
 * effect it runs on the ambient runtime.
 */
class ToTryTimeoutSpec extends AnyFunSuite with Matchers {

  // longer than `ToTry` waits; a passing run never reaches it
  private val slowRecovery = 1.minute

  test("a timeout releases what the effect acquired and gives the permit back") {
    val io = for {
      guard <- Semaphore[IO](1)
      released <- Ref[IO].of(false)
      // the permit is taken on the calling thread; the state rebuild then sleeps past the timeout
      recovery = guard.permit.use { _ =>
        Resource.onFinalize(released.set(true)).use(_ => IO.sleep(slowRecovery))
      }
      outcome <- IO.blocking { ToTry.ioToTry(50.millis).apply(recovery) }
      wasReleased <- released.get
      permits <- guard.available
    } yield {
      outcome should matchPattern { case Failure(_: TimeoutException) => }
      wasReleased shouldBe true
      permits shouldEqual 1L
    }
    io.unsafeRunSync()
  }

  test("a retry after a timed-out attempt can take the permit again") {
    val io = for {
      guard <- Semaphore[IO](1)
      toTry = ToTry.ioToTry(50.millis)
      recovery = guard.permit.use(_ => IO.sleep(slowRecovery))
      first <- IO.blocking { toTry(recovery) }
      // what a retry around the flow does next
      second <- IO.blocking { toTry(recovery) }
      permits <- guard.available
    } yield {
      first should matchPattern { case Failure(_: TimeoutException) => }
      second should matchPattern { case Failure(_: TimeoutException) => }
      permits shouldEqual 1L
    }
    io.unsafeRunSync()
  }

  test("an uncancelable guarded recovery runs past the timeout and gives the permit back") {
    val io = for {
      guard <- Semaphore[IO](1)
      cancelled <- Ref[IO].of(false)
      // `kafka-flow` guards `add`, `apply` and its own release with one permit, taken inside
      // `uncancelable`: a permit lost to cancellation would block all three
      recovery = guard
        .permit
        .use(_ => IO.sleep(200.millis).onCancel(cancelled.set(true)))
        .uncancelable
      outcome <- IO.blocking { ToTry.ioToTry(50.millis).apply(recovery) }
      wasCancelled <- cancelled.get
      permits <- guard.available
    } yield {
      outcome shouldEqual Success(())
      wasCancelled shouldBe false
      permits shouldEqual 1L
    }
    io.unsafeRunSync()
  }

  test("nested uncancelable: a timeout reaches a polled region and the permit comes back") {
    val io = for {
      guard <- Semaphore[IO](1)
      cancelled <- Ref[IO].of(false)
      // the sleep sits under two `uncancelable`, both polled, so it stays cancelable. The step stops
      // at the outermost one and returns the whole nest, which `IO.timeout` treats like any other `IO`
      recovery = guard.permit.use { _ =>
        IO.uncancelable { outer =>
          outer(IO.uncancelable(inner => inner(IO.sleep(slowRecovery).onCancel(cancelled.set(true)))))
        }
      }
      outcome <- IO.blocking { ToTry.ioToTry(50.millis).apply(recovery) }
      wasCancelled <- cancelled.get
      permits <- guard.available
    } yield {
      outcome should matchPattern { case Failure(_: TimeoutException) => }
      wasCancelled shouldBe true
      permits shouldEqual 1L
    }
    io.unsafeRunSync()
  }
}
