package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import cats.effect.IO

import scala.concurrent.Future

trait ToFuture[F[_]] {

  def apply[A](fa: F[A]): Future[A]
}

object ToFuture {

  def apply[F[_]](implicit F: ToFuture[F]): ToFuture[F] = F

  def summon[F[_]](implicit F: ToFuture[F]): ToFuture[F] = F


  def functionK[F[_]: ToFuture]: FunctionK[F, Future] = new FunctionK[F, Future] {

    def apply[A](fa: F[A]) = ToFuture.summon[F].apply(fa)
  }


  implicit val ioToFuture: ToFuture[IO] = new ToFuture[IO] {
    def apply[A](fa: IO[A]) = fa.unsafeToFuture()
  }


  implicit val idToFuture: ToFuture[Id] = new ToFuture[Id] {
    def apply[A](fa: Id[A]) = Future.successful(fa)
  }

  implicit val futureToFuture: ToFuture[Future] = new ToFuture[Future] {
    def apply[A](fa: Future[A]) = fa
  }


  implicit class ToFutureOps[F[_]](val self: ToFuture[F]) extends AnyVal {

    def toFunctionK: FunctionK[F, Future] = functionK(self)
  }
}