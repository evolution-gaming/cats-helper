package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import cats.effect.IO

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait ToTry[F[_]] {

  def apply[A](fa: F[A]): Try[A]
}

object ToTry {

  @deprecated("use `summon` instead", "2.0.2")
  def apply[F[_]](implicit F: ToTry[F]): ToTry[F] = F

  def summon[F[_]](implicit F: ToTry[F]): ToTry[F] = F


  def functionK[F[_] : ToTry]: FunctionK[F, Try] = new FunctionK[F, Try] {

    def apply[A](fa: F[A]): Try[A] = ToTry.summon[F].apply(fa)
  }


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


  implicit val tryToTry: ToTry[Try] = new ToTry[Try] {
    def apply[A](fa: Try[A]) = fa
  }
}