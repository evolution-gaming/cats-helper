package com.evolutiongaming.catshelper

import cats.effect.{Async, Sync}
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait FromFuture[F[_]] {

  def apply[A](future: => Future[A]): F[A]
}

object FromFuture {

  def apply[F[_]](implicit F: FromFuture[F]): FromFuture[F] = F


  implicit def lift[F[_] : Async](implicit executor: ExecutionContext): FromFuture[F] = {

    new FromFuture[F] {

      def apply[A](future: => Future[A]) = {
        for {
          future <- Sync[F].delay(future)
          result <- future.value.fold {
            Async[F].async[A] { callback =>
              future.onComplete { a =>
                callback(a.toEither)
              }
            }
          } {
            case Success(a) => a.pure[F]
            case Failure(a) => a.raiseError[F, A]
          }
        } yield result
      }
    }
  }
}