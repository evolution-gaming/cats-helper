package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.implicits._
import cats.{ApplicativeError, Id}

import scala.concurrent.Future
import scala.util.Try

trait FromTry[F[_]] {

  def apply[A](fa: Try[A]): F[A]

  final def unsafe[A](a: => A): F[A] = apply(Try(a))
}

object FromTry {

  def apply[F[_]](implicit F: FromTry[F]): FromTry[F] = F


  def lift[F[_]](implicit F: ApplicativeError[F, Throwable]): FromTry[F] = new FromTry[F] {
    def apply[A](fa: Try[A]) = F.fromTry(fa)
  }


  implicit val ioFromTry: FromTry[IO] = lift


  implicit val idFromTry: FromTry[Id] = new FromTry[Id] {
    def apply[A](fa: Try[A]) = fa.get
  }


  implicit val futureFromTry: FromTry[Future] = new FromTry[Future] {
    def apply[A](fa: Try[A]) = Future.fromTry(fa)
  }


  implicit val tryFromTry: FromTry[Try] = lift
}