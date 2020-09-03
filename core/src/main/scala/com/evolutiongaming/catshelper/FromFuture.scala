package com.evolutiongaming.catshelper

import cats.arrow.FunctionK
import cats.effect.{Async, Sync}
import cats.syntax.all._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait FromFuture[F[_]] {

  def apply[A](future: => Future[A]): F[A]
}

object FromFuture {

  def apply[F[_]](implicit F: FromFuture[F]): FromFuture[F] = F

  def summon[F[_]](implicit F: FromFuture[F]): FromFuture[F] = F


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


  def functionK[F[_] : FromFuture]: FunctionK[Future, F] = new FunctionK[Future, F] {

    def apply[A](fa: Future[A]) = FromFuture.summon[F].apply(fa)
  }


  implicit class FromFutureOps[F[_]](val self: FromFuture[F]) extends AnyVal {

    def toFunctionK: FunctionK[Future, F] = functionK(self)
  }
}