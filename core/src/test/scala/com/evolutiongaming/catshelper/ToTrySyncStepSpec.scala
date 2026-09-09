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
 * Pins where `ioToTry` runs an effect: a pure or `delay`-shaped effect on the calling thread,
 * everything else as a fiber. The calling-thread cases are asserted against a runtime whose
 * executors throw instead of running a fiber, so submitting one fails the conversion and cannot
 * pass unnoticed.
 *
 * What the timeout then does to the fiber is [[ToTryTimeoutSpec]].
 */
class ToTrySyncStepSpec extends AnyFunSuite with Matchers {

  private val toTry = ToTry.ioToTry(100.millis)(runtimeRejectingFibers)

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

  // The `true` shapes are what a per-record conversion runs, a codec or a deserializer, and they
  // have to stay on the calling thread. The `false` shapes are the ones the step must not enter:
  // entering them is what dropped the mask and the finalizers, so reaching the runtime is the
  // evidence that the step stopped in front of them instead.
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
   * Runs nothing: every fiber submission throws [[FiberSubmitted]] out of the conversion.
   */
  private def runtimeRejectingFibers: IORuntime = {
    val rejecting = new ExecutionContext {
      def execute(runnable: Runnable): Unit = throw FiberSubmitted
      def reportFailure(cause: Throwable): Unit = ()
    }
    IORuntime(rejecting, rejecting, ioRuntime.scheduler, () => (), IORuntimeConfig())
  }
}
