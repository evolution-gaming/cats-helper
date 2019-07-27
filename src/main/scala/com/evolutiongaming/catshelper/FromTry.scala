package com.evolutiongaming.catshelper

import cats.Id
import cats.effect.IO

import scala.util.Try

trait FromTry[F[_]] {

  def apply[A](fa: Try[A]): F[A]
}

object FromTry {

  def apply[F[_]](implicit F: FromTry[F]): FromTry[F] = F

  implicit val ioFromTry: FromTry[IO] = new FromTry[IO] {
    def apply[A](fa: Try[A]) = IO.fromTry(fa)
  }


  implicit val idToTry: FromTry[Id] = new FromTry[Id] {
    def apply[A](fa: Try[A]) = fa.get
  }
}