package com.evolutiongaming.catshelper

import cats.Id
import cats.effect.IO

import scala.concurrent.Future
import scala.util.Try

trait FromTry[F[_]] {

  def apply[A](fa: Try[A]): F[A]

  final def unsafe[A](a: => A): F[A] = apply(Try(a))
}

object FromTry {

  def apply[F[_]](implicit F: FromTry[F]): FromTry[F] = F

  implicit val ioFromTry: FromTry[IO] = new FromTry[IO] {
    def apply[A](fa: Try[A]) = IO.fromTry(fa)
  }


  implicit val idFromTry: FromTry[Id] = new FromTry[Id] {
    def apply[A](fa: Try[A]) = fa.get
  }


  implicit val futureFromTry: FromTry[Future] = new FromTry[Future] {
    def apply[A](fa: Try[A]) = Future.fromTry(fa)
  }


  implicit val tryFromTry: FromTry[Try] = new FromTry[Try] {
    def apply[A](fa: Try[A]) = fa
  }
}