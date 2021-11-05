package com.evolutiongaming.catshelper

import cats.effect.kernel.{Deferred, Fiber}
import cats.effect.{Concurrent, MonadCancel}
import cats.implicits._
import cats.effect.implicits._

import scala.concurrent.Future
import scala.util.Try

@deprecated("use CatsHelper instead", "1.0.1")
object EffectHelper {

  @deprecated("use BracketOpsEffectHelper", "1.0.0")
  class EffectHelperBracketOps[F[_], E](val self: MonadCancel[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] = {
      val fb = self.map(fa)(map)
      self.handleError(fb)(recover)
    }

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], flatMap: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(flatMap)
      self.handleErrorWith(fb)(recover)
    }
  }


  implicit class BracketOpsEffectHelper[F[_], E](val self: MonadCancel[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] = {
      val fb = self.map(fa)(map)
      self.handleError(fb)(recover)
    }

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], flatMap: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(flatMap)
      self.handleErrorWith(fb)(recover)
    }
  }


  @deprecated("use ConcurrentOpsEffectHelper instead", "1.0.0")
  class EffectHelperConcurrentOps[F[_]](val self: Concurrent[F]) extends AnyVal {

    def startEnsure[A](fa: F[A]): F[Fiber[F, Throwable, A]] = {
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


  implicit class ConcurrentOpsEffectHelper[F[_]](val self: Concurrent[F]) extends AnyVal {

    def startEnsure[A](fa: F[A]): F[Fiber[F, Throwable, A]] = {
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


  @deprecated("use FOpsEffectHelper instead", "1.0.0")
  class EffectHelperFOps[F[_], A](val self: F[A]) extends AnyVal {

    def redeem[B, E](recover: E => B, map: A => B)(implicit bracket: MonadCancel[F, E]): F[B] = {
      bracket.redeem(self)(recover, map)
    }

    def redeemWith[B, E](recover: E => F[B], flatMap: A => F[B])(implicit bracket: MonadCancel[F, E]): F[B] = {
      bracket.redeemWith(self)(recover, flatMap)
    }


    def startEnsure(implicit F: Concurrent[F]): F[Fiber[F, Throwable, A]] = F.startEnsure(self)


    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)


    def toFuture(implicit F: ToFuture[F]): Future[A] = F.apply(self)
  }


  implicit class FOpsEffectHelper[F[_], A](val self: F[A]) extends AnyVal {

    def redeem[B, E](recover: E => B, map: A => B)(implicit bracket: MonadCancel[F, E]): F[B] = {
      bracket.redeem(self)(recover, map)
    }

    def redeemWith[B, E](recover: E => F[B], flatMap: A => F[B])(implicit bracket: MonadCancel[F, E]): F[B] = {
      bracket.redeemWith(self)(recover, flatMap)
    }


    def startEnsure(implicit F: Concurrent[F]): F[Fiber[F, Throwable, A]] = F.startEnsure(self)


    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)


    def toFuture(implicit F: ToFuture[F]): Future[A] = F.apply(self)
  }


  implicit class EffectHelperTryOps[A](val self: Try[A]) extends AnyVal {

    def fromTry[F[_]](implicit fromTry: FromTry[F]): F[A] = fromTry(self)
  }
}
