package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import cats.effect.IO

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import cats.effect.unsafe.IORuntime

trait ToTry[F[_]] {

  def apply[A](fa: F[A]): Try[A]
}

object ToTry {

  def apply[F[_]](implicit F: ToTry[F]): ToTry[F] = F

  def summon[F[_]](implicit F: ToTry[F]): ToTry[F] = F


  def functionK[F[_] : ToTry]: FunctionK[F, Try] = new FunctionK[F, Try] {

    def apply[A](fa: F[A]): Try[A] = ToTry.summon[F].apply(fa)
  }


  /**
    * Please think twice before using this, ideally you should not have toTry in your `pure` code base!
    *
    * @param timeout used only for computation after first seen async boundary, covering all computations onwards
    *                in case there is no async boundary found, timeout is not used
    */
  def ioToTry(timeout: FiniteDuration)(implicit runtime: IORuntime): ToTry[IO] = new ToTry[IO] {

    def apply[A](fa: IO[A]) = {

      def error: Try[A] = Failure[A](new TimeoutException(timeout.toString()))

      for {
        a <- Try {
          fa.syncStep.unsafeRunSync() match {
            case Left(computation) =>
              computation.unsafeRunTimed(timeout)
            case Right(value) =>
              Some(value)
          }
        }
        a <- a.fold(error) { a => Success(a) }
      } yield a
    }
  }


  implicit def ioToTry(implicit ioRuntime: IORuntime): ToTry[IO] = ioToTry(1.minute)


  implicit val idToTry: ToTry[Id] = new ToTry[Id] {
    def apply[A](fa: Id[A]) = Success(fa)
  }


  implicit val tryToTry: ToTry[Try] = new ToTry[Try] {
    def apply[A](fa: Try[A]) = fa
  }
}