package com.evolutiongaming.catshelper

import cats.Id
import cats.effect.IO

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Try}

trait ToTry[F[_]] {

  def apply[A](fa: F[A]): Try[A]
}

object ToTry {

  def apply[F[_]](implicit F: ToTry[F]): ToTry[F] = F


  def ioToTry(timeout: FiniteDuration): ToTry[IO] = new ToTry[IO] {

    def apply[A](fa: IO[A]) = {
      val future = fa.unsafeToFuture
      Try { Await.result(future, timeout) }
    }
  }


  implicit val ioToTry: ToTry[IO] = ioToTry(1.minute)


  implicit val idToTry: ToTry[Id] = new ToTry[Id] {
    def apply[A](fa: Id[A]) = Success(fa)
  }


  implicit val tryToTry: ToTry[Try] = new ToTry[Try] {
    def apply[A](fa: Try[A]) = fa
  }
}