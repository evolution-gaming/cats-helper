package com.evolutiongaming.catshelper.testkit

import cats.effect.implicits._
import cats.effect.testkit.TestContext
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Sync}
import cats.syntax.all._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import cats.effect.Temporal

private[testkit] object PureTestRunner {
  type TestBody[F[A], A] = PureTest.Env[F] => F[A]

  def doRunTest[F[_] : Async : UnliftIO, A](body: TestBody[F, A], config: PureTest.Config) = {
    val singleRun = wrap(body, config)

    val fullTestIO = config.flakinessCheckIterations match {
      case n if n > 0 => singleRun.replicateA(n).map(_.head)
      case _          => singleRun
    }

    fullTestIO.unsafeRunSync()
  }

  private def wrap[F[_] : Async : UnliftIO, A](body: TestBody[F, A], config: PureTest.Config): IO[A] = IO.defer {
    val env = new EnvImpl[F]

    @volatile var outcome: Option[Either[Throwable, A]] = None
    val testIo = UnliftIO[F].unliftIO(body(env).evalOn(env.testContext)).attempt map { r =>
      IO.delay { outcome = Some(r) }
    }
    val cancel = testIo.start.unsafeRunSync().cancel

    val testThread = Thread.currentThread()

    val stopHotLoop = IO.delay {
      val err = new IllegalStateException("Still running")
      err.setStackTrace(testThread.getStackTrace)
      outcome = Some(Left(err))
    } *> cancel

    val hotLoopGuard = IO.sleep(config.hotLoopTimeout)

    val timeoutCancel = (hotLoopGuard *> stopHotLoop).start
      .evalOn(config.backgroundEc).unsafeRunSync().cancel

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

    implicit val testRuntime: TestRuntime[F] = new TestRuntime[F] {

      /** NB: We exploit the fact that TestContext starts from 0. */
      def getTimeSinceStart: F[FiniteDuration] = Sync[F].delay(testContext.state.clock)

      def sleepUntil(dt: FiniteDuration): F[Unit] =
        getTimeSinceStart.flatMap(t => Temporal[F].sleep(dt - t).whenA(dt > t))
    }
  }
}


