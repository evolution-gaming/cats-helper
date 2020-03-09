package com.evolutiongaming.catshelper.testkit

import cats.effect.{ContextShift, Effect, IO, Timer}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

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
object PureTest extends PureTest {
  /** An environment that is injected into every test. */
  trait Env[F[_]] {
    implicit def ec: ExecutionContext
    implicit def cs: ContextShift[F]
    implicit def timer: Timer[F]
    implicit def testRuntime: TestRuntime[F]
  }

  private val default: PureTest = Impl(Config())

  protected def config = default.config
  protected def withConfigMod(f: Config => Config) = default.withConfigMod(f)

  private case class Impl(config: Config) extends PureTest {
    protected def withConfigMod(f: Config => Config) = copy(f(config))
  }

  private[testkit] case class Config(
    testFrameworkApi: TestFrameworkApi = TestFrameworkApi.resolveDefault,
    backgroundEc: ExecutionContext = ExecutionContext.global,
    hotLoopTimeout: FiniteDuration = 10.seconds,
    flakinessCheckIterations: Int = 1,
  )
}

/**
 * Holds a test configuration. May be configured further or used to build and run a test.
 */
sealed trait PureTest { self =>
  import PureTest._

  /**
   * A follow-up to [[apply `PureTest[F]`]].
   */
  final class PartialApply[F[_]: Effect] {
    /**
     * Builds and runs the test with previously defined settings.
     */
    def of[A](body: Env[F] => F[A]): A = PureTestRunner.doRunTest(body, config)
  }

  /**
   * A first half of [[PartialApply.of `PureTest[F].of`]].
   *
   * @see [[ioTest `ioTest`]] – a shortcut for `IO`-based tests.
   */
  final def apply[F[_]: Effect]: PartialApply[F] = new PartialApply[F]

  /**
   * A shorter version of [[PartialApply.of `PureTest[IO].of`]].
   */
  final def ioTest[A](body: Env[IO] => IO[A]): A = PureTestRunner.doRunTest(body, config)

  protected def config: Config

  protected def withConfigMod(f: Config => Config): PureTest

  /**
   * Sets a test-framework integration layer. Currently defaults to ScalaTest.
   */
  final def testFrameworkApi(v: TestFrameworkApi): PureTest = withConfigMod(_.copy(testFrameworkApi = v))

  /**
   * Sets hot-loop detection timeout. Use it CPU-bound part of your SUT or test requires longer time.
   */
  final def hotLoopTimeout(v: FiniteDuration): PureTest = withConfigMod(_.copy(hotLoopTimeout = v))

  /**
   * Sets an `ExecutionContext` used for background tasks, such as hot loop detection.
   * Defaults to `ExecutionContext.global`.
   */
  final def backgroundEc(v: ExecutionContext): PureTest = withConfigMod(_.copy(backgroundEc = v))

  /**
   * Sets a number of times to run a test to make sure it's not flaky. Defaults to `1`.
   */
  final def flakinessCheckIterations(v: Int): PureTest = withConfigMod(_.copy(flakinessCheckIterations = v))
}
