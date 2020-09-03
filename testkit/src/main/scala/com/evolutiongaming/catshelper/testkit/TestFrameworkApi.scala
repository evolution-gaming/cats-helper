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

object TestFrameworkApi {
  /**
   * Returns either a default API (ScalaTest as of now) if it's available on the classpath.
   * Uses [[NoFrameworkApi]] as a fallback.
   */
  def resolveDefault(implicit l: Lookup): TestFrameworkApi = l.instance

  private[testkit] trait Lookup {
    def instance: TestFrameworkApi
  }

  private[testkit] object Lookup extends LookupLowPriority {
    implicit def scalaTestIsDefault(implicit ev: TestFailedException =:= TestFailedException): Lookup = {
      if (ev == null) () else ()  // A kludge to avoid "unused" warning :(
      new Lookup {
        def instance: TestFrameworkApi = ScalaTestApi
      }
    }
  }

  private[testkit] sealed trait LookupLowPriority {
    implicit def fallbackToNoFramework: Lookup = new Lookup {
      def instance: TestFrameworkApi = NoFrameworkApi
    }
  }
}

/**
 * A stub implementation to use when no test framework is available.
 */
object NoFrameworkApi extends TestFrameworkApi {
  def completeWith[A](outcome: Option[Either[Throwable, A]], tcState: TestContext.State): IO[A] = {
    IO.fromEither(outcome.toRight(new IllegalStateException(s"Not completed. State: $tcState")).flatten)
  }
}

/**
 * ScalaTest integration.
 */
private[testkit] object ScalaTestApi extends TestFrameworkApi {
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
      case None           => boo(Right("Not completed"))
    }
  }
}
