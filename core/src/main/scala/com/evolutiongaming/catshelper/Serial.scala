package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.implicits._
import cats.implicits._

trait Serial[F[_]] {

  /**
    * @return outer F[_] is about adding fa to queue, inner F[_] is about fa completed
    */
  def apply[A](fa: F[A]): F[F[A]]
}

object Serial {

  def of[F[_]: Concurrent]: F[Serial[F]] = {

    Ref[F]
      .of(().pure[F])
      .map { ref =>

        new Serial[F] {

          def apply[A](fa: F[A]): F[F[A]] = {

            Concurrent[F].uncancelable {
              for {
                deferred <- Deferred.uncancelable[F, Either[Throwable, A]]
                gate     <- Deferred.uncancelable[F, Unit]
                next     = gate.get
                prev     <- ref.modify { prev => (next, prev) }
                task = for {
                  _ <- prev
                  a <- fa.attempt
                  _ <- gate.complete(())
                  a <- deferred.complete(a)
                } yield a
                _        <- task.start
              } yield {
                deferred.get.rethrow
              }
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