package com.evolutiongaming.catshelper

import cats.effect.kernel.Async
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.syntax.all._

/**
 * Runs tasks strictly one after another, in submission order.
 *
 * Unlike a mutex-based approach the caller does not wait for its turn: `apply` only registers the
 * task and returns. Tasks run on a background fiber, started on demand and released when the queue
 * drains.
 *
 * Registration is uncancelable: once `apply` returns, the task runs even if the caller is canceled.
 * Canceling the inner effect stops the waiting, not the task.
 *
 * A task that cancels itself stops the runner before it advances the queue, so every task behind it
 * waits forever, see https://github.com/evolution-gaming/cats-helper/issues/404
 *
 * See [[SerialKey]] for per-key serialization and [[SerialRef]] for serialized access to a value.
 */
trait Serial[F[_]] {

  /**
   * @return
   *   outer F[_] is about adding `fa` to the queue, inner F[_] is about `fa` being completed. If
   *   `fa` fails, the inner F[_] raises its error and the queue continues with the next task.
   */
  def apply[A](fa: F[A]): F[F[A]]
}

object Serial {

  /**
   * Pending tasks are chained into a single `F[Unit]` via `productR` rather than held in a
   * collection, so the runner claims the whole backlog in one `ref.modify` instead of one per task.
   * Enqueue stays O(1). Memory grows with the backlog either way.
   */
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
                      case _ => (S.Idle, ().asRight[F[Unit]])
                    }
                  } yield a
                }
                .start
                .void
            }

            val result = for {
              d <- Deferred[F, Either[Throwable, A]]
              t = fa.attempt.flatMap { a => d.complete(a).void }
              r <- ref.modify {
                case S.Idle => (S.Active, start(t))
                case S.Active => (S.Active(t), Async[F].unit)
                case S.Active(a) => (S.Active(a.productR(t)), Async[F].unit)
              }
              _ <- r
            } yield
              for {
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

      def serial(
        implicit
        serial: Serial[F],
      ): F[F[A]] = serial(self)
    }
  }
}
