package com.evolutiongaming.catshelper.testkit

import scala.concurrent.duration.FiniteDuration

/**
 * Provides access to running test info.
 */
trait TestRuntime[F[_]] {
  def getTimeSinceStart: F[FiniteDuration]
}

object TestRuntime {
  def apply[F[_]](implicit summonned: TestRuntime[F]): TestRuntime[F] = summonned
}
