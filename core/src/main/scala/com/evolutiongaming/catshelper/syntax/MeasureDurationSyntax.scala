package com.evolutiongaming.catshelper.syntax

import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.{Monad, MonadError}
import com.evolutiongaming.catshelper.MeasureDuration

import scala.concurrent.duration.FiniteDuration


trait MeasureDurationSyntax {
  implicit def measureDurationSyntax[F[_], A](fa: F[A]): MeasureDurationOps[F, A] =
    new MeasureDurationOps[F, A](fa)
}

final class MeasureDurationOps[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * Calculate time spent on F execution and handle result using provided function.
   *
   * Example:
   * {{{
   * import com.evolutiongaming.smetrics.syntax.measureDuration._
   * val handler: FiniteDuration => F[Unit] = latency => Sync[F].delay { println(s"Successful execution. Latency is \$latency") }
   * Sync[F]
   *   .delay { toBeMeasuredSideEffectCall() }
   *   .measured { handler }
   * }}}
   *
   * @param handleF function to consume calculated duration
   * @return measured source F[A]
   */
  def measured(handleF: FiniteDuration => F[Unit])
              (implicit F: Monad[F], measureDuration: MeasureDuration[F]): F[A] =
    for {
      measure <- measureDuration.start
      result <- fa
      duration <- measure
      _ <- handleF(duration)
    } yield result

  /**
   * Calculate time spent on F execution and handle success/failure result cases using provided functions.
   *
   * Example:
   * {{{
   * import com.evolutiongaming.smetrics.syntax.measureDuration._
   * val successHandler: FiniteDuration => F[Unit] = latency => Sync[F].delay { println(s"Successful execution. Latency is \$latency") }
   * val failureHandler: FiniteDuration => F[Unit] = latency => Sync[F].delay { println(s"Failed execution. Latency is \$latency") }
   * Sync[F]
   *   .delay { toBeMeasuredSideEffectCall() }
   *   .measuredCase { successHandler, failureHandler }
   * }}}
   *
   * @param successF function to consume calculated duration in case of success
   * @param failureF function to consume calculated duration in case of failure
   * @return measured source F[A]
   */
  def measuredCase[E](successF: FiniteDuration => F[Unit],
                      failureF: FiniteDuration => F[Unit])
                     (implicit F: MonadError[F, E], measureDuration: MeasureDuration[F]): F[A] =
    for {
      measure <- measureDuration.start
      result <- fa.attempt
      duration <- measure
      _ <- result match {
        case Right(_) => successF(duration)
        case Left(_)  => failureF(duration)
      }
      result <- result.liftTo[F]
    } yield result

}
