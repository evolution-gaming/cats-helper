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
 * `skafka` runs a Kafka rebalance callback through `ToTry[IO]`, and `kafka-flow` holds a semaphore
 * permit across its partition recovery as `semaphore.permit.use { ... }.uncancelable`. These tests
 * pin that a timeout runs finalizers, gives the permit back, and waits for an uncancelable
 * recovery. The previous `ioToTry` did none of the three (kafka-flow#937).
 */
class ToTryTimeoutSpec extends AnyFunSuite with Matchers {

  // longer than `ToTry` waits; a passing run never reaches it
  private val slowRecovery = 1.minute

  test("a timeout releases what the effect acquired and gives the permit back") {
    val io = for {
      guard <- Semaphore[IO](1)
      released <- Ref[IO].of(false)
      recovery = guard.permit.use { _ =>
        Resource.onFinalize(released.set(true)).use(_ => IO.sleep(slowRecovery))
      }
      // IO.blocking because the conversion blocks the calling thread, as skafka does on the poll thread
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
      // permit inside `uncancelable`, as kafka-flow does it
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
      // two uncancelable, both polled: the sleep stays cancelable
      recovery = guard.permit.use { _ =>
        IO.uncancelable { outer =>
          outer(IO.uncancelable { inner =>
            inner(IO.sleep(slowRecovery).onCancel(cancelled.set(true)))
          })
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
