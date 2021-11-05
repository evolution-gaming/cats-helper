package com.evolutiongaming.catshelper.testkit

import cats.effect.IO
import cats.effect.kernel.Async

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
object PureTest extends PureTest[IO] {
  /** An environment that is injected into every test. */
  trait Env[F[_]] {
    implicit def async: Async[F]
    implicit def testRuntime: TestRuntime[F]
  }

  private val default: PureTest[IO] = Impl(Config())

  protected def config = default.config
  protected def withConfigMod(f: Config[IO] => Config[IO]) = default.withConfigMod(f)

  private case class Impl(config: Config[IO]) extends PureTest[IO] {
    protected def withConfigMod(f: Config[IO] => Config[IO]) = copy(f(config))
  }

  private[testkit] case class Config[F[_]](
    testFrameworkApi: TestFrameworkApi[F] = TestFrameworkApi.resolveDefault,
    backgroundEc: ExecutionContext = ExecutionContext.global,
    hotLoopTimeout: FiniteDuration = 10.seconds,
    flakinessCheckIterations: Int = 1,
  )

  implicit class Ops(val T: PureTest[IO]) extends AnyVal {
    final def ioTest[A](body: Env[IO] => IO[A]): A = PureTest.ioTest(body, T.config)
  }

  final def ioTest[A](body: Env[IO] => IO[A]): A = {
    ioTest(body, config)
  }

  private def ioTest[A](body: Env[IO] => IO[A], testConfig: PureTest.Config[IO]): A = {
    val ioRuntime = cats.effect.unsafe.implicits.global

    PureTestRunner.doRunTest(body, testConfig, ioRuntime).unsafeRunSync()(ioRuntime)
  }
}

/**
 * Holds a test configuration. May be configured further or used to build and run a test.
 */
sealed trait PureTest[F[_]] { self =>
  import PureTest._

  protected def config: Config[F]

  protected def withConfigMod(f: Config[F] => Config[F]): PureTest[F]

  /**
   * Sets a test-framework integration layer. Currently defaults to ScalaTest.
   */
  final def testFrameworkApi(v: TestFrameworkApi[F]): PureTest[F] = withConfigMod(_.copy(testFrameworkApi = v))

  /**
   * Sets hot-loop detection timeout. Use it CPU-bound part of your SUT or test requires longer time.
   */
  final def hotLoopTimeout(v: FiniteDuration): PureTest[F] = withConfigMod(_.copy(hotLoopTimeout = v))

  /**
   * Sets an `ExecutionContext` used for background tasks, such as hot loop detection.
   * Defaults to `ExecutionContext.global`.
   */
  final def backgroundEc(v: ExecutionContext): PureTest[F] = withConfigMod(_.copy(backgroundEc = v))

  /**
   * Sets a number of times to run a test to make sure it's not flaky. Defaults to `1`.
   */
  final def flakinessCheckIterations(v: Int): PureTest[F] = withConfigMod(_.copy(flakinessCheckIterations = v))
}
