package com.evolutiongaming.catshelper

import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{IO, Resource}
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

/**
 * Pins where `ioToTry` runs an effect: pure and `delay`-shaped effects inline on the calling
 * thread, everything else as a fiber under `IO.timeout`, and that the timeout honours masks and
 * brackets. The inline path is asserted against a runtime whose executors throw on `execute`, so
 * any fiber submission fails the conversion.
 */
class ToTrySyncStepSpec extends AnyFunSuite with Matchers {

  private val timeout = 100.millis

  private val toTry = ToTry.ioToTry(timeout)

  private val toTryNoRuntime = ToTry.ioToTry(timeout)(poisoned)

  test("pure and delay-shaped effects never touch the runtime") {
    val effect = IO.pure(1).map(_ + 1).flatMap(x => IO(x * 2)).handleErrorWith(_ => IO.pure(-1))

    toTryNoRuntime(effect) shouldEqual Success(4)
  }

  test("a 100k-deep flatMap chain over Ref.update steps to completion without a fiber") {
    val effect = IO.ref(0).flatMap { ref =>
      (1 to 100_000).foldLeft(IO.unit)((acc, _) => acc.flatMap(_ => ref.update(_ + 1))) *> ref.modify(n => (n, n))
    }

    toTryNoRuntime(effect) shouldEqual Success(100_000)
  }

  for {
    (name, effect, inline) <- List(
      ("Ref#update", IO.ref(0).flatMap(_.update(_ + 1)), true),
      ("Deferred#complete", IO.deferred[Int].flatMap(_.complete(1).void), true),
      ("uncancelable", IO.unit.uncancelable, false),
      ("onCancel", IO.unit.onCancel(IO.unit), false),
      ("Resource.allocated", Resource.make(IO.unit)(_ => IO.unit).allocated.void, false),
    )
  } {
    test(s"$name ${ if (inline) "runs inline" else "goes through the runtime" }") {
      val expected = if (inline) Success(()) else Failure(RuntimeTouched)

      toTryNoRuntime(effect) shouldEqual expected
    }
  }

  test("the timeout waits inside an uncancelable region") {
    val cancelled = new AtomicBoolean(false)
    val finished = new AtomicBoolean(false)
    val body = IO.sleep(300.millis).onCancel(IO(cancelled.set(true))) *> IO(finished.set(true))
    val started = System.nanoTime()

    toTry(body.uncancelable) shouldEqual Success(())

    (System.nanoTime() - started).nanos should be >= 300.millis
    finished.get() shouldBe true
    cancelled.get() shouldBe false
  }

  test("the timeout releases a resource acquired before the async boundary") {
    val acquired = new AtomicBoolean(false)
    val released = new AtomicBoolean(false)
    val effect = Resource
      .make(IO(acquired.set(true)))(_ => IO(released.set(true)))
      .use(_ => IO.sleep(10.seconds))

    toTry(effect) should matchPattern { case Failure(_: TimeoutException) => }

    acquired.get() shouldBe true
    released.get() shouldBe true
  }

  private object RuntimeTouched extends RuntimeException("IORuntime was touched") with NoStackTrace

  private def poisoned: IORuntime = {
    val throwing = new ExecutionContext {
      def execute(runnable: Runnable): Unit = throw RuntimeTouched
      def reportFailure(cause: Throwable): Unit = ()
    }
    IORuntime(throwing, throwing, ioRuntime.scheduler, () => (), IORuntimeConfig())
  }
}
