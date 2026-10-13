package com.evolutiongaming.catshelper

import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{IO, Resource}
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Success
import scala.util.control.NoStackTrace

/**
 * `ioToTry` runs simple effects on the calling thread and everything else as a fiber. The runtime
 * here rejects fibers, so submitting one fails the conversion. [[ToTryTimeoutSpec]] covers the
 * timeout behaviour of the fiber part.
 */
class ToTrySyncStepSpec extends AnyFunSuite with Matchers {

  // the timeout is never reached: a fiber submission fails before it could matter
  private val toTry = ToTry.ioToTry(1.minute)(runtimeRejectingFibers)

  test("pure and delay-shaped effects never touch the runtime") {
    val effect = IO.pure(1).map(_ + 1).flatMap(x => IO(x * 2)).handleErrorWith(_ => IO.pure(-1))

    toTry(effect) shouldEqual Success(4)
  }

  test("a 100k-deep flatMap chain over Ref#update steps to completion without a fiber") {
    val effect = IO.ref(0).flatMap { ref =>
      (1 to 100_000).foldLeft(IO.unit)((acc, _) => acc.flatMap(_ => ref.update(_ + 1))) *> ref.get
    }

    toTry(effect) shouldEqual Success(100_000)
  }

  for {
    (name, effect) <- List(
      ("Ref#update", IO.ref(0).flatMap(_.update(_ + 1))),
      ("Deferred#complete", IO.deferred[Int].flatMap(_.complete(1).void)),
      ("uncancelable", IO.unit.uncancelable),
      ("onCancel", IO.unit.onCancel(IO.unit)),
      ("Resource.allocated", Resource.make(IO.unit)(_ => IO.unit).allocated.void),
    )
  } {
    test(s"$name runs on the calling thread") {
      toTry(effect) shouldEqual Success(())
    }
  }

  for {
    (name, effect) <- List(
      ("uncancelable + cede", IO.uncancelable(_ => IO.cede)),
      ("onCancel + cede", IO.cede.onCancel(IO.unit)),
      ("Resource.use + cede", Resource.make(IO.unit)(_ => IO.unit).use(_ => IO.cede)),
    )
  } {
    test(s"$name hits an async boundary and goes through the runtime") {
      toTry(effect).isFailure shouldBe true
    }
  }

  private object FiberSubmitted extends RuntimeException("a fiber was submitted to the runtime") with NoStackTrace

  /**
   * A runtime that runs nothing: submitting a fiber throws [[FiberSubmitted]].
   */
  private def runtimeRejectingFibers: IORuntime = {
    val rejecting = new ExecutionContext {
      def execute(runnable: Runnable): Unit = throw FiberSubmitted
      def reportFailure(cause: Throwable): Unit = ()
    }
    IORuntime(rejecting, rejecting, ioRuntime.scheduler, () => (), IORuntimeConfig())
  }
}
