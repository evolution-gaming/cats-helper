package com.evolutiongaming.catshelper.testkit

import cats.effect.IO
import cats.effect.laws.util.TestContext
import org.scalatest.exceptions.{TestCanceledException, TestFailedException}

/**
 * EXPERIMENTAL. Abstracts interactions with a test framework.
 */
trait TestFrameworkApi {
  /**
   * Signals a completion of a test.
   *
   * @param outcome an outcome which can be non-termination, failure, or success.
   * @param tcState `TestContext` state at the moment of completion.
   */
  def completeWith[A](outcome: Option[Either[Throwable, A]], tcState: TestContext.State): IO[A]
}

object ScalaTestApi extends TestFrameworkApi {
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
