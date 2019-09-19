package com.evolutiongaming.catshelper

import cats.{ApplicativeError, MonadError}
import cats.effect.concurrent.Deferred
import cats.effect.{Bracket, Concurrent, Fiber}
import cats.implicits._
import cats.effect.implicits._

import scala.concurrent.Future
import scala.util.Try

object EffectHelper {

  @deprecated("use BracketOpsEffectHelper", "1.0.0")
  class EffectHelperBracketOps[F[_], E](val self: Bracket[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] = {
      self.redeem(fa)(recover, map)
    }

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], flatMap: A => F[B]): F[B] = {
      self.redeemWith(fa)(recover, flatMap)
    }
  }


  @deprecated("use ApplicativeErrorOpsEffectHelper or MonadErrorOpsEffectHelper instead", "1.0.1")
  class BracketOpsEffectHelper[F[_], E](val self: Bracket[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, map: A => B): F[B] = {
      self.redeem(fa)(recover, map)
    }

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], flatMap: A => F[B]): F[B] = {
      self.redeemWith(fa)(recover, flatMap)
    }
  }


  implicit class ApplicativeErrorOpsEffectHelper[F[_], E](val self: ApplicativeError[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, ab: A => B): F[B] = {
      val fb = self.map(fa)(ab)
      self.handleError(fb)(recover)
    }
  }


  implicit class MonadErrorOpsEffectHelper[F[_], E](val self: MonadError[F, E]) extends AnyVal {

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], ab: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(ab)
      self.handleErrorWith(fb)(recover)
    }
  }


  @deprecated("use ConcurrentOpsEffectHelper instead", "1.0.0")
  class EffectHelperConcurrentOps[F[_]](val self: Concurrent[F]) extends AnyVal {
    
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


  implicit class ConcurrentOpsEffectHelper[F[_]](val self: Concurrent[F]) extends AnyVal {

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


  @deprecated("use FOpsEffectHelper instead", "1.0.0")
  class EffectHelperFOps[F[_], A](val self: F[A]) extends AnyVal {

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


  @deprecated("use FOpsEffectHelper instead", "1.0.1")
  class FOpsEffectHelper[F[_], A](val self: F[A]) extends AnyVal {

    def redeem[B, E](recover: E => B, map: A => B)(implicit bracket: Bracket[F, E]): F[B] = {
      self.redeem(recover, map)
    }

    def redeemWith[B, E](recover: E => F[B], flatMap: A => F[B])(implicit bracket: Bracket[F, E]): F[B] = {
      self.redeemWith(recover, flatMap)
    }


    def startEnsure(implicit F: Concurrent[F]): F[Fiber[F, A]] = self.startEnsure


    def toTry(implicit F: ToTry[F]): Try[A] = self.toTry


    def toFuture(implicit F: ToFuture[F]): Future[A] = self.toFuture
  }


  implicit class OpsEffectHelper[F[_], A](val self: F[A]) extends AnyVal {

    def redeem[B, E](recover: E => B, ab: A => B)(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeem(self)(recover, ab)
    }

    def redeemWith[B, E](recover: E => F[B], ab: A => F[B])(implicit bracket: Bracket[F, E]): F[B] = {
      bracket.redeemWith(self)(recover, ab)
    }


    def startEnsure(implicit F: Concurrent[F]): F[Fiber[F, A]] = F.startEnsure(self)


    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)


    def toFuture(implicit F: ToFuture[F]): Future[A] = F.apply(self)
  }


  implicit class EffectHelperTryOps[A](val self: Try[A]) extends AnyVal {

    def fromTry[F[_]](implicit fromTry: FromTry[F]): F[A] = fromTry(self)
  }
}
