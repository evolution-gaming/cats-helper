package com.evolutiongaming.catshelper

import cats.effect.IO

import scala.concurrent.Future

trait ToFuture[F[_]] {

  def apply[A](fa: F[A]): Future[A]
}

object ToFuture {

  def apply[F[_]](implicit F: ToFuture[F]): ToFuture[F] = F


  implicit val io: ToFuture[IO] = new ToFuture[IO] {
    def apply[A](fa: IO[A]) = fa.unsafeToFuture()
  }
}