package com.evolutiongaming.catshelper

import cats.Id
import cats.effect.IO

import scala.concurrent.Future

trait ToFuture[F[_]] {

  def apply[A](fa: F[A]): Future[A]
}

object ToFuture {

  def apply[F[_]](implicit F: ToFuture[F]): ToFuture[F] = F


  @deprecated("use ioToFuture instead", "0.0.10")
  val io: ToFuture[IO] = new ToFuture[IO] {
    def apply[A](fa: IO[A]) = fa.unsafeToFuture()
  }


  implicit val ioToFuture: ToFuture[IO] = new ToFuture[IO] {
    def apply[A](fa: IO[A]) = fa.unsafeToFuture()
  }


  implicit val idToFuture: ToFuture[Id] = new ToFuture[Id] {
    def apply[A](fa: Id[A]) = Future.successful(fa)
  }
}