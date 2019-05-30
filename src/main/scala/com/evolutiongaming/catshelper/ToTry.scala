package com.evolutiongaming.catshelper

import cats.Id
import cats.effect.IO

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait ToTry[F[_]] {

  def apply[A](fa: F[A]): Try[A]
}

object ToTry {

  def apply[F[_]](implicit F: ToTry[F]): ToTry[F] = F


  def ioToTry(timeout: FiniteDuration): ToTry[IO] = new ToTry[IO] {

    def apply[A](fa: IO[A]) = {

      def error: Try[A] = Failure[A](new TimeoutException(timeout.toString()))

      for {
        a <- Try { fa.unsafeRunTimed(timeout) }
        a <- a.fold(error) { a => Success(a) }
      } yield a
    }
  }


  implicit val ioToTry: ToTry[IO] = ioToTry(1.minute)


  implicit val idToTry: ToTry[Id] = new ToTry[Id] {
    def apply[A](fa: Id[A]) = Success(fa)
  }
}