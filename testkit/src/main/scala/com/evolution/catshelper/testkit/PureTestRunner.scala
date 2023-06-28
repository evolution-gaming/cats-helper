package com.evolution.catshelper.testkit

import cats.effect.testkit.{TestContext, TestInstances}
import cats.effect.unsafe.IORuntime
import cats.effect.{Async, IO}
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.duration._

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

    // Test should be run against IORuntime which was build with `TestContext`
    // which we control below.
    val (testResult: Future[A], testCancel: (() => Future[Unit])) =
      body(env).unsafeToFutureCancelable()(env.ioRuntime)

    // In the background we schedule a cancellation task to avoid stalling the test
    // if the test IO goes into an infinite flatMap chain.
    val timeoutCancel: () => Future[Unit] = {
      val stopHotLoop = IO.fromFuture(IO.delay(testCancel())).delayBy(config.hotLoopTimeout)
      stopHotLoop.unsafeRunCancelable()(mainRuntime)
    }

    while (!testResult.isCompleted && env.testContext.state.tasks.nonEmpty) {
      val nextInterval = env.testContext.nextInterval()
      if (nextInterval > Duration.Zero) {
        env.testContext.advanceAndTick(nextInterval)
      } else {
        env.testContext.tick()
      }
    }

    // Cancel the background cancellation task if it's still pending.
    timeoutCancel()

    val result = testResult.value.map(_.toEither)
    config.testFrameworkApi.completeWith(result, env.testContext.state)
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


