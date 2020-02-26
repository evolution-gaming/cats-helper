package com.evolutiongaming.catshelper.testkit

import cats.effect.implicits._
import cats.effect.laws.util.TestContext
import cats.effect.{Async, ContextShift, Effect, IO, LiftIO, Sync, Timer}
import cats.implicits._
import cats.{Id, ~>}
import org.scalatest.exceptions.{TestCanceledException, TestFailedException}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/**
 * Provides a boilerplate for writing "pure FP" tests (usually using `IO` and for-comprehensions).
 *
 * Time in tests is simulated. Clocks "move" only with time-based actions (such as `IO.sleep(…)`)
 * while anything else is perceived to happen "immediately". Corollary:
 *  - you can use `IO.sleep` with arbitrary delays to test concurrent behaviour, scheduling, etc.
 *  - no matter how long are the durations in your test, the test itself runs as fast as possible.
 *
 * Another virtue of `PureTest` is that can terminate "hot loops" that do infinite monadic binds,
 * e.g. `IO.unit.foreverM`. It does this by cancelling the running test after a "wall-clock" delay.
 * This may introduce test flakiness for long CPU-intensive scenarios, when your hardware is stressed,
 * but for such cases you have control over the hot loop cancellation timeout.
 *
 * Keep in mind though, that hot loop detector is not a silver bullet. It can't help against
 * `while (true)`, neither against infinite binds in `uncancelable` regions.
 *
 * @example {{{
 *   "what time is it now?" in PureTest[IO].of { env =>
 *     import env._
 *     for {
 *       _ <- IO.sleep(1.hour)
 *       _ <- testRuntime.getTimeSinceStart.map(_ shouldBe 1.hour)
 *     } yield ()
 *   }
 * }}}
 */
object PureTest {
  /** An environment that is injected into every test. */
  trait Env[F[_]] {
    implicit def ec: ExecutionContext
    implicit def cs: ContextShift[F]
    implicit def timer: Timer[F]
    implicit def testRuntime: TestRuntime[F]
  }

  type TestBody[F[_], A] = Env[F] => F[A]
  type RunTest[F[_]] = TestBody[F, *] ~> Id

  /**
   * First step of building your test.
   *
   * @see [[PartialApply the rest of the builder]]
   * @see [[ioTest:* `ioTest`]] – a shortcut for `IO`-based tests.
   */
  def apply[F[_] : Effect]: PartialApply[F] = new PartialApply[F]

  /** A follow-up to [[apply `PureTest[F]`]]. */
  final class PartialApply[F[_] : Effect] {
    /** Builds and runs the test with default settings. */
    def of: RunTest[F] = of(defaultHotLoopTimeout)

    /**
     * Builds and runs the test. You may customise its settings.
     *
     * @param hotLoopTimeout – a time it takes to decide that test is spinning in a hot loop and kill it.
     */
    def of(hotLoopTimeout: FiniteDuration = defaultHotLoopTimeout): RunTest[F] =
      Lambda[TestBody[F, *] ~> Id](body => runTest(body, hotLoopTimeout))
  }

  /** A shorter version of [[PartialApply.of:* `PureTest[IO].of`]]. */
  def ioTest: RunTest[IO] = PureTest[IO].of

  /** A shorter version of [[PartialApply.of(* `PureTest[IO].of(…)`]]. */
  def ioTest(hotLoopTimeout: FiniteDuration = defaultHotLoopTimeout): RunTest[IO] =
    PureTest[IO].of(hotLoopTimeout)

  /** A default timeout for hot loop detection. */
  def defaultHotLoopTimeout: FiniteDuration = 10.seconds

  private def runTest[F[_] : Effect, A](body: TestBody[F, A], hotLoopTimeout: FiniteDuration) = {
    val testControl = new TestControl(
      hotLoopGuard = IO.timer(ExecutionContext.global).sleep(hotLoopTimeout),
    )
    val testIO = wrap(body, testControl)
    testIO.unsafeRunSync()
  }

  private def wrap[F[_] : Effect, A](body: TestBody[F, A], testControl: TestControl): IO[A] = IO.suspend {
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

    val timeoutCancel = (testControl.hotLoopGuard *> stopHotLoop).unsafeRunCancelable(_ => ())

    while (outcome.isEmpty && env.testContext.state.tasks.nonEmpty) {
      val step = env.testContext.state.tasks.iterator.map(_.runsAt).min
      env.testContext.tick(step)
    }

    timeoutCancel.unsafeRunSync()

    testControl.completeWith(outcome, env.testContext.state)
  }

  private class EnvImpl[F[_] : Async : LiftIO] extends Env[F] {
    val testContext: TestContext = TestContext()

    implicit def ec: ExecutionContext = testContext
    implicit val cs: ContextShift[F] = testContext.contextShift[F]
    implicit val timer: Timer[F] = testContext.timer[F]

    implicit val testRuntime: TestRuntime[F] = new TestRuntime[F] {

      /** NB: We exploit the fact that TestContext starts from 0. */
      def getTimeSinceStart: F[FiniteDuration] = Sync[F].delay(testContext.state.clock)
    }
  }

  private class TestControl(val hotLoopGuard: IO[Unit]) {
    def completeWith[A](outcome: Option[Either[Throwable, A]], tcState: TestContext.State): IO[A] = IO {
      def boo(cause: Either[Throwable, String]): Nothing = {
        val err = cause match {
          case Left(e: TestFailedException)   => e
          case Left(e: TestCanceledException) => e
          case abnormal                       => AbnormalTermination(abnormal, tcState)
        }
        throw err
      }

      outcome match {
        case Some(Right(a)) => a
        case Some(Left(e))  => boo(Left(e))
        case None           => boo(Right(s"Not completed. State: $tcState"))
      }
    }
  }

  final case class AbnormalTermination(
    cause: Either[Throwable, String],
    tcState: TestContext.State,
  ) extends RuntimeException(s"${ cause.fold(_.toString, identity) }. $tcState", cause.left.toOption.orNull)
    with NoStackTrace
}
