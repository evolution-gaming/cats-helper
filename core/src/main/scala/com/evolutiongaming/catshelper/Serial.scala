package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.implicits._
import cats.syntax.all._

trait Serial[F[_]] {

  /**
    * @return outer F[_] is about adding fa to queue, inner F[_] is about fa completed
    */
  def apply[A](fa: F[A]): F[F[A]]
}

object Serial {

  def of[F[_]: Concurrent]: F[Serial[F]] = {

    Ref[F]
      .of(none[List[F[Unit]]])
      .map { ref =>

        new Serial[F] {

          def apply[A](fa: F[A]) = {

            def start(f: F[Unit]): F[Unit] = {
              Nel
                .of(f)
                .tailRecM[F, Unit] { fs =>
                  fs
                    .reverse
                    .foldMapM(identity)
                    .productR {
                      ref.modify {
                        case Some(f :: fs) => (List.empty[F[Unit]].some, Nel(f, fs).asLeft[Unit])
                        case _             => (none[List[F[Unit]]], ().asRight[Nel[F[Unit]]])
                      }
                    }
                }
                .start
                .void
            }

            Concurrent[F].uncancelable {
              for {
                d <- Deferred.uncancelable[F, Either[Throwable, A]]
                f  = fa.attempt.flatMap { a => d.complete(a) }
                r <- ref.modify {
                  case Some(fs) => ((f :: fs).some, ().pure[F])
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