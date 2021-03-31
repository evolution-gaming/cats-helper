package com.evolutiongaming.catshelper.testkit

import cats.effect.laws.util.TestContext
import cats.effect.{Async, ContextShift, Effect, IO, Sync, Timer}
import cats.effect.implicits._
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

private[testkit] object PureTestRunner {
  type TestBody[F[A], A] = PureTest.Env[F] => F[A]

  def doRunTest[F[_] : Effect, A](body: TestBody[F, A], config: PureTest.Config) = {
    val singleRun = wrap(body, config)

    val fullTestIO = config.flakinessCheckIterations match {
      case n if n > 0 => singleRun.replicateA(n).map(_.head)
      case _          => singleRun
    }

    fullTestIO.unsafeRunSync()
  }

  private def wrap[F[_] : Effect, A](body: TestBody[F, A], config: PureTest.Config): IO[A] = IO.suspend {
    val env = new EnvImpl[F]

    val testIo = (env.cs.shift *> body(env)).toIO

    @volatile var outcome: Option[Either[Throwable, A]] = None
    val cancel = testIo.unsafeRunCancelable { r => outcome = Some(r) }

    val testThread = Thread.currentThread()

    val stopHotLoop = IO.suspend {
      val err = new IllegalStateException("Still running")
      err.setStackTrace(testThread.getStackTrace)
      outcome = Some(Left(err))
      cancel
    }

    val hotLoopGuard = IO.timer(config.backgroundEc).sleep(config.hotLoopTimeout)

    val timeoutCancel = (hotLoopGuard *> stopHotLoop).unsafeRunCancelable(_ => ())

    while (outcome.isEmpty && env.testContext.state.tasks.nonEmpty) {
      val step = env.testContext.state.tasks.iterator.map(_.runsAt).min
      env.testContext.tick(step)
    }

    timeoutCancel.unsafeRunSync()

    config.testFrameworkApi.completeWith(outcome, env.testContext.state)
  }

  private class EnvImpl[F[_] : Async] extends PureTest.Env[F] {
    val testContext: TestContext = TestContext()

    implicit def ec: ExecutionContext = testContext
    implicit val cs: ContextShift[F] = testContext.contextShift[F]
    implicit val timer: Timer[F] = testContext.timer[F]

    implicit val testRuntime: TestRuntime[F] = new TestRuntime[F] {

      /** NB: We exploit the fact that TestContext starts from 0. */
      def getTimeSinceStart: F[FiniteDuration] = Sync[F].delay(testContext.state.clock)

      def sleepUntil(dt: FiniteDuration): F[Unit] =
        getTimeSinceStart.flatMap(t => timer.sleep(dt - t).whenA(dt > t))
    }
  }
}


