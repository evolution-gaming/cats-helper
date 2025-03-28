package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.IORuntime

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait ToFuture[F[_]] {

  def apply[A](fa: F[A]): Future[A]
}

object ToFuture {

  def apply[F[_]](implicit F: ToFuture[F]): ToFuture[F] = F

  def summon[F[_]](implicit F: ToFuture[F]): ToFuture[F] = F


  def functionK[F[_]: ToFuture]: FunctionK[F, Future] = new FunctionK[F, Future] {

    def apply[A](fa: F[A]) = ToFuture.summon[F].apply(fa)
  }


  implicit def ioToFuture(implicit runtime: IORuntime): ToFuture[IO] = new ToFuture[IO] {
    def apply[A](fa: IO[A]) = {
      // `limit` can be adjusted with Cats-Effect config `cats.effect.auto.yield.threshold.multiplier`
      Try(fa.syncStep(limit = runtime.config.autoYieldThreshold).unsafeRunSync()) match {
        case Success(Left(computation)) =>
          computation.unsafeToFuture()
        case Success(Right(value)) =>
          Future.successful(value)
        case Failure(error) =>
          Future.failed(error)
      }
    }
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