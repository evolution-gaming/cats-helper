package com.evolutiongaming.catshelper

import cats.effect.Bracket

import scala.util.Try

object EffectHelper {

  implicit class BracketOps[F[_], E](val self: Bracket[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] = {
      val fb = self.map(fa)(map)
      self.handleError(fb)(recover)
    }

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], flatMap: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(flatMap)
      self.handleErrorWith(fb)(recover)
    }
  }


  implicit class FOps[F[_], A](val self: F[A]) extends AnyVal {

    def redeem[B, E](recover: E => B, map: A => B)(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeem(self)(recover, map)
    }

    def redeemWith[B, E](recover: E => F[B], flatMap: A => F[B])(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeemWith(self)(recover, flatMap)
    }

    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)
  }
}
