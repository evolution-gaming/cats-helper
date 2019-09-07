package com.evolutiongaming.catshelper

import cats.effect.concurrent.Deferred
import cats.effect.{Bracket, Concurrent, Fiber}
import cats.implicits._
import cats.effect.implicits._

import scala.concurrent.Future
import scala.util.Try

object EffectHelper {

  @deprecated("use EffectHelperBracketOps", "0.0.18")
  class BracketOps[F[_], E](val self: Bracket[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] = {
      val fb = self.map(fa)(map)
      self.handleError(fb)(recover)
    }

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], flatMap: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(flatMap)
      self.handleErrorWith(fb)(recover)
    }
  }


  implicit class EffectHelperBracketOps[F[_], E](val self: Bracket[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] = {
      val fb = self.map(fa)(map)
      self.handleError(fb)(recover)
    }

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], flatMap: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(flatMap)
      self.handleErrorWith(fb)(recover)
    }
  }


  implicit class EffectHelperConcurrentOps[F[_]](val self: Concurrent[F]) extends AnyVal {
    
    def startEnsure[A](fa: F[A]): F[Fiber[F, A]] = {
      implicit val F = self

      def faOf(started: Deferred[F, Unit]) = {
        for {
          _ <- started.complete(())
          a <- fa
        } yield a
      }

      for {
        started <- Deferred[F, Unit]
        fiber   <- faOf(started).start
        _       <- started.get
      } yield fiber
    }
  }


  @deprecated("use EffectHelperFOps", "0.0.18")
  class FOps[F[_], A](val self: F[A]) extends AnyVal {

    def redeem[B, E](recover: E => B, map: A => B)(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeem(self)(recover, map)
    }

    def redeemWith[B, E](recover: E => F[B], flatMap: A => F[B])(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeemWith(self)(recover, flatMap)
    }


    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)


    def toFuture(implicit F: ToFuture[F]): Future[A] = F.apply(self)
  }


  implicit class EffectHelperFOps[F[_], A](val self: F[A]) extends AnyVal {

    def redeem[B, E](recover: E => B, map: A => B)(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeem(self)(recover, map)
    }

    def redeemWith[B, E](recover: E => F[B], flatMap: A => F[B])(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeemWith(self)(recover, flatMap)
    }


    def startEnsure(implicit F: Concurrent[F]): F[Fiber[F, A]] = F.startEnsure(self)


    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)


    def toFuture(implicit F: ToFuture[F]): Future[A] = F.apply(self)
  }


  implicit class EffectHelperTryOps[A](val self: Try[A]) extends AnyVal {

    def fromTry[F[_]](implicit fromTry: FromTry[F]): F[A] = fromTry(self)
  }
}
