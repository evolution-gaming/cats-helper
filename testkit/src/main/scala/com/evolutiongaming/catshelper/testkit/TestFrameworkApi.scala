package com.evolutiongaming.catshelper.testkit

import cats.effect.IO
import cats.effect.testkit.TestContext
import org.scalatest.exceptions.{TestCanceledException, TestFailedException}

/**
 * EXPERIMENTAL. Abstracts interactions with a test framework.
 */
trait TestFrameworkApi[F[_]] {
  /**
   * Signals a completion of a test.
   *
   * @param outcome an outcome which can be non-termination, failure, or success.
   * @param tcState `TestContext` state at the moment of completion.
   */
  def completeWith[A](outcome: Option[Either[Throwable, A]], tcState: TestContext.State): F[A]
}

object TestFrameworkApi {
  /**
   * Returns either a default API (ScalaTest as of now) if it's available on the classpath.
   * Uses [[NoFrameworkApi]] as a fallback.
   */
  def resolveDefault[F[_]](implicit l: Lookup[F]): TestFrameworkApi[F] = l.instance
}

  private[testkit] trait Lookup[F[_]] {
    def instance: TestFrameworkApi[F]
  }

  private[testkit] object Lookup extends LookupLowPriority {
    implicit def scalaTestIsDefault(implicit ev: TestFailedException =:= TestFailedException): Lookup[IO] = {
      if (ev == null) () else ()  // A kludge to avoid "unused" warning :(
      new Lookup[IO] {
        def instance: TestFrameworkApi[IO] = ScalaTestApi
      }
    }
  }

  private[testkit] sealed trait LookupLowPriority {
    implicit def fallbackToNoFramework: Lookup[IO] = new Lookup[IO] {
      def instance: TestFrameworkApi[IO] = NoFrameworkApi
    }
  }

/**
 * A stub implementation to use when no test framework is available.
 */
object NoFrameworkApi extends TestFrameworkApi[IO] {
  def completeWith[A](outcome: Option[Either[Throwable, A]], tcState: TestContext.State): IO[A] = {
    IO.fromEither(outcome.toRight(new IllegalStateException(s"Not completed. State: $tcState")).flatten)
  }
}

/**
 * ScalaTest integration.
 */
private[testkit] object ScalaTestApi extends TestFrameworkApi[IO] {
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
