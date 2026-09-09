package com.evolutiongaming.catshelper

import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{IO, Resource}
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

/**
 * Pins where `ioToTry` runs an effect: pure and `delay`-shaped ones on the calling thread,
 * everything else as a fiber. The runtime used here throws instead of running a fiber, so a
 * conversion that submits one fails the test.
 *
 * [[ToTryTimeoutSpec]] covers what the timeout does to the fiber.
 */
class ToTrySyncStepSpec extends AnyFunSuite with Matchers {

  // the timeout is never reached here: a fiber submission fails at once
  private val toTry = ToTry.ioToTry(1.minute)(runtimeRejectingFibers)

  test("pure and delay-shaped effects never touch the runtime") {
    val effect = IO.pure(1).map(_ + 1).flatMap(x => IO(x * 2)).handleErrorWith(_ => IO.pure(-1))

    toTry(effect) shouldEqual Success(4)
  }

  test("a 100k-deep flatMap chain over Ref.update steps to completion without a fiber") {
    val effect = IO.ref(0).flatMap { ref =>
      (1 to 100_000).foldLeft(IO.unit)((acc, _) => acc.flatMap(_ => ref.update(_ + 1))) *> ref.modify(n => (n, n))
    }

    toTry(effect) shouldEqual Success(100_000)
  }

  // `true` shapes are what a per-record codec or deserializer runs; they must stay on the calling
  // thread. `false` shapes are what the step must not enter, because entering them drops the mask
  // and the finalizers. Reaching the runtime proves it stopped in front of them.
  for {
    (name, effect, callingThread) <- List(
      ("Ref#update", IO.ref(0).flatMap(_.update(_ + 1)), true),
      ("Deferred#complete", IO.deferred[Int].flatMap(_.complete(1).void), true),
      ("uncancelable", IO.unit.uncancelable, false),
      ("onCancel", IO.unit.onCancel(IO.unit), false),
      ("Resource.allocated", Resource.make(IO.unit)(_ => IO.unit).allocated.void, false),
    )
  } {
    test(s"$name ${ if (callingThread) "runs on the calling thread" else "goes through the runtime" }") {
      val expected = if (callingThread) Success(()) else Failure(FiberSubmitted)

      toTry(effect) shouldEqual expected
    }
  }

  private object FiberSubmitted extends RuntimeException("a fiber was submitted to the runtime") with NoStackTrace

  /**
   * Runs nothing: a fiber submission throws [[FiberSubmitted]], which becomes the result.
   */
  private def runtimeRejectingFibers: IORuntime = {
    val rejecting = new ExecutionContext {
      def execute(runnable: Runnable): Unit = throw FiberSubmitted
      def reportFailure(cause: Throwable): Unit = ()
    }
    IORuntime(rejecting, rejecting, ioRuntime.scheduler, () => (), IORuntimeConfig())
  }
}
