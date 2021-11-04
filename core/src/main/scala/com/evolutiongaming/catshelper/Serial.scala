package com.evolutiongaming.catshelper

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.implicits._
import cats.effect.{Concurrent, Sync}
import cats.implicits._

trait Serial[F[_]] {

  /**
    * @return outer F[_] is about adding fa to queue, inner F[_] is about fa completed
    */
  def apply[A](fa: F[A]): F[F[A]]
}

object Serial {

  def of[F[_]: Concurrent]: F[Serial[F]] = {

    val void = ().pure[F]

    Ref[F]
      .of(none[List[F[Unit]]])
      .map { ref =>

        new Serial[F] {

          def apply[A](fa: F[A]) = {

            def start(task: F[Unit]): F[Unit] = {
              task
                .tailRecM[F, Unit] { task =>
                  for {
                    _ <- task
                    a <- ref.modify {
                      case Some(tasks) if tasks.nonEmpty =>
                        val task = Sync[F].defer { tasks.reverse.sequence_ }
                        (List.empty[F[Unit]].some, task.asLeft[Unit])
                      case _                             =>
                        (none[List[F[Unit]]], ().asRight[F[Unit]])
                    }
                  } yield a
                }
                .start
                .void
            }

            Concurrent[F].uncancelable {
              for {
                d <- Deferred.uncancelable[F, Either[Throwable, A]]
                f  = fa.attempt.flatMap { a => d.complete(a) }
                r <- ref.modify {
                  case Some(fs) => ((f :: fs).some, void)
                  case None     => (List.empty[F[Unit]].some, start(f))
                }
                _ <- r
              } yield for {
                a <- d.get
                a <- a.liftTo[F]
              } yield a
            }
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