package com.evolutiongaming.catshelper

import cats.effect.kernel.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.syntax.all._
import cats.effect.kernel.Async

/**
 * Allows running effects serially, so only one effect can be running at a given moment of time.
 * Also preserves the order of effects. It's possible to wait for the effect to complete
 * or just enqueue it and forget.
 *
 * Example:
 * {{{
 *  for {
 *    serial  <- Serial.of[IO]
 *
 *    waitF1  <- serial.apply(f1) // f1 is guaranteed to run before f2, because f1 is enqueued first
 *    _       <- serial.apply(f2) // don't wait for f2 to complete
 *    _       <- waitF1           // wait for f1 to complete, f2 may still be running
 *  } yield ()
 * }}}
 */
trait Serial[F[_]] {

  /**
    * @return outer F[_] is about adding `fa` to the queue, inner F[_] is about `fa` being completed
    */
  def apply[A](fa: F[A]): F[F[A]]
}

object Serial {

  def of[F[_]: Async]: F[Serial[F]] = {

    sealed trait S

    object S {
      case object Idle extends S
      case object Active extends S
      final case class Active(task: F[Unit]) extends S
    }

    Ref[F]
      .of[S](S.Idle)
      .map { ref =>
        new Serial[F] {

          def apply[A](fa: F[A]) = {

            def start(task: F[Unit]): F[Unit] = {
              task
                .tailRecM[F, Unit] { task =>
                  for {
                    _ <- task
                    a <- ref.modify {
                      case S.Active(a) => (S.Active, a.asLeft[Unit])
                      case _           => (S.Idle, ().asRight[F[Unit]])
                    }
                  } yield a
                }
                .start
                .void
            }

            val result = for {
              d <- Deferred[F, Either[Throwable, A]]
              t  = fa.attempt.flatMap { a => d.complete(a).void }
              r <- ref.modify {
                case S.Idle       => (S.Active, start(t))
                case S.Active     => (S.Active(t), Async[F].unit)
                case S.Active(a)  => (S.Active(a.productR(t)), Async[F].unit)
              }
              _ <- r
            } yield for {
              a <- d.get
              a <- a.liftTo[F]
            } yield a
            result.uncancelable
          }
        }
      }
  }


  object implicits {

    implicit class OpsSerial[F[_], A](val self: F[A]) extends AnyVal {

      def serial(implicit serial: Serial[F]): F[F[A]] = serial(self)
    }
  }
}