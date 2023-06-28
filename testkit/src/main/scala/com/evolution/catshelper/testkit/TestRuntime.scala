package com.evolution.catshelper.testkit

import scala.concurrent.duration.FiniteDuration

/**
 * Provides access to running test info.
 */
trait TestRuntime[F[_]] {
  /**
   * Returns time on ''test'' clock passed since the start of the test.
   */
  def getTimeSinceStart: F[FiniteDuration]

  /**
   * Assuming that tests start at `0`, sleep until it's `dt` time into the test.
   *
   * It's roughly equivalent to `getTimeSinceStart.flatMap(t => timer.sleep(dt - t))`
   * but does not require a `timer: Timer[F]` argument.
   */
  def sleepUntil(dt: FiniteDuration): F[Unit]
}

object TestRuntime {
  def apply[F[_]](implicit summonned: TestRuntime[F]): TestRuntime[F] = summonned
}
