package com.evolutiongaming.catshelper.testkit

import cats.effect.kernel.Outcome
import cats.effect.testkit.{TestContext, TestInstances}
import cats.effect.unsafe.IORuntime
import cats.effect.{Async, IO}
import cats.implicits._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

private[testkit] object PureTestRunner {
  type TestBody[A] = PureTest.Env[IO] => IO[A]

  def doRunTest[A](body: TestBody[A], config: PureTest.Config[IO], mainRuntime: IORuntime): IO[A] = {
    val singleRun = wrap(body, config, mainRuntime)

    val fullTestIO = config.flakinessCheckIterations match {
      case n if n > 0 => singleRun.replicateA(n).map(_.head)
      case _ => singleRun
    }

    fullTestIO
  }

  private def wrap[A](body: TestBody[A], config: PureTest.Config[IO], mainRuntime: IORuntime): IO[A] = IO.defer {
    val env = new EnvImpl[IO]
    @volatile var outcome: Option[Either[Throwable, A]] = None

    val testF = body(env).guaranteeCase {
      case Outcome.Canceled() => IO.delay {
        outcome = Some(Left(new IllegalStateException("Canceled")))
      }
      case Outcome.Errored(e) => IO.delay {
        outcome = Some(Left(e))
      }
      case Outcome.Succeeded(value) => value.map { v => outcome = Some(Right(v)) }
    }

    // test should be run against IORuntime which was build with `TestContext`
    val cancel: () => Future[Unit] = testF.unsafeRunCancelable()(env.ioRuntime)

    val testThread = Thread.currentThread()
    val stopHotLoop = IO.defer {
      val err = new IllegalStateException("Still running")
      err.setStackTrace(testThread.getStackTrace)
      outcome = Some(Left(err))
      IO.fromFuture(IO.delay(cancel()))
    }
    val hotLoopGuard = IO.sleep(config.hotLoopTimeout)
    val timeoutCancel: () => Future[Unit] = (hotLoopGuard *> stopHotLoop).unsafeRunCancelable()(mainRuntime)

    while (outcome.isEmpty && env.testContext.state.tasks.nonEmpty) {
      // TestContext's clock now starts from a very negative value, so the next step should be a diff between current clock value and the next task
      val currentClock = env.testContext.state.clock
      val nextClock = env.testContext.state.tasks.iterator.map(_.runsAt).min
      val step = nextClock - currentClock
      env.testContext.tick(step)
    }

    Await.ready(timeoutCancel(), 10.seconds) // cancel hot loop guard in case we finished successfully

    config.testFrameworkApi.completeWith(outcome, env.testContext.state)
  }

  private[testkit] class EnvImpl[F[_]](implicit val async: Async[F]) extends PureTest.Env[F] with TestInstances {
    val testContext: TestContext = TestContext()

    val ioRuntime: IORuntime = materializeRuntime(Ticker(testContext))

    implicit val testRuntime: TestRuntime[F] = new TestRuntime[F] {

      // TestContext in CE3 starts from a very negative value
      private val startClock = testContext.state.clock

      def getTimeSinceStart: F[FiniteDuration] = async.delay(testContext.state.clock - startClock)

      def sleepUntil(dt: FiniteDuration): F[Unit] =
        getTimeSinceStart.flatMap(t => async.sleep(dt - t).whenA(dt > t))
    }
  }
}


