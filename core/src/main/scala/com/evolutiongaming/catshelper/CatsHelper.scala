package com.evolutiongaming.catshelper

import cats.data.EitherT
import cats.effect.concurrent.Deferred
import cats.effect.implicits._
import cats.effect.{Concurrent, Fiber, Resource}
import cats.implicits._
import cats.{Applicative, ApplicativeError, MonadError}

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Either, Try}

object CatsHelper extends CatsSyntax

trait CatsSyntax {

  import CatsSyntaxOps._

  implicit def toApplicativeErrorOpsCatsHelper[F[_], E](self: ApplicativeError[F, E]): ApplicativeErrorOpsCatsHelper[F, E] = new ApplicativeErrorOpsCatsHelper(self)

  implicit def toMonadErrorOpsCatsHelper[F[_], E](self: MonadError[F, E]): MonadErrorOpsCatsHelper[F, E] = new MonadErrorOpsCatsHelper(self)

  implicit def toConcurrentOpsCatsHelper[F[_]](self: Concurrent[F]): ConcurrentOpsCatsHelper[F] = new ConcurrentOpsCatsHelper(self)

  implicit def toIdOpsCatsHelper[A](self: A): IdOpsCatsHelper[A] = new IdOpsCatsHelper(self)

  implicit def toOpsCatsHelper[F[_], A](self: F[A]): OpsCatsHelper[F, A] = new OpsCatsHelper(self)

  implicit def toTryOpsCatsHelper[A](self: Try[A]): TryOpsCatsHelper[A] = new TryOpsCatsHelper(self)

  implicit def toResourceOpsCatsHelper[F[_], A](self: Resource[F, A]): ResourceOpsCatsHelper[F, A] = new ResourceOpsCatsHelper(self)

  implicit def toResourceObjOpsCatsHelper[F[_]](self: Resource.type): ResourceObjOpsCatsHelper = new ResourceObjOpsCatsHelper(self)

  implicit def toBooleanOpsCatsHelper[F[_]](self: Boolean): BooleanOpsCatsHelper = new BooleanOpsCatsHelper(self)

  implicit def toBooleanOpsTrueOrFApply[F[_]](self: Boolean): BooleanOpsTrueOrFApply[F] = new BooleanOpsTrueOrFApply[F](self)

  implicit def toBooleanOpsFalseOrFApply[F[_]](self: Boolean): BooleanOpsFalseOrFApply[F] = new BooleanOpsFalseOrFApply[F](self)
}

private[catshelper] object CatsSyntaxOps {
  class ApplicativeErrorOpsCatsHelper[F[_], E](val self: ApplicativeError[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, ab: A => B): F[B] = {
      val fb = self.map(fa)(ab)
      self.handleError(fb)(recover)
    }
  }


  class MonadErrorOpsCatsHelper[F[_], E](val self: MonadError[F, E]) extends AnyVal {

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], ab: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(ab)
      self.handleErrorWith(fb)(recover)
    }
  }


  implicit class ConcurrentOpsCatsHelper[F[_]](val self: Concurrent[F]) extends AnyVal {

    def startEnsure[A](fa: F[A]): F[Fiber[F, A]] = {
      implicit val F: Concurrent[F] = self

      def faOf(started: Deferred[F, Unit]) = {
        for {
          _ <- started.complete(())
          a <- fa
        } yield a
      }

      for {
        started <- Deferred[F, Unit]
        fiber <- faOf(started).start
        _ <- started.get
      } yield fiber
    }
  }


  class IdOpsCatsHelper[A](val self: A) extends AnyVal {

    def castM[F[_] : MonadThrowable, B <: A](implicit tag: ClassTag[B]): F[B] = {

      def error = new ClassCastException(s"${self.getClass.getName} cannot be cast to ${tag.runtimeClass.getName}")

      castOpt[B].fold {
        error.raiseError[F, B]
      } {
        _.pure[F]
      }
    }


    def castOpt[B <: A](implicit tag: ClassTag[B]): Option[B] = tag.unapply(self)
  }


  implicit class OpsCatsHelper[F[_], A](val self: F[A]) extends AnyVal {

    def startEnsure(implicit F: Concurrent[F]): F[Fiber[F, A]] = F.startEnsure(self)


    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)


    def toFuture(implicit F: ToFuture[F]): Future[A] = F.apply(self)


    def toResource(implicit F: Applicative[F]): Resource[F, A] = Resource.eval(self)
  }


  class TryOpsCatsHelper[A](val self: Try[A]) extends AnyVal {

    def fromTry[F[_]](implicit fromTry: FromTry[F]): F[A] = fromTry(self)
  }


  class ResourceOpsCatsHelper[F[_], A](val self: Resource[F, A]) extends AnyVal {

    def fenced(implicit F: Concurrent[F]): Resource[F, A] = ResourceFenced(self)

    def semiflatMap[B, G[x] >: F[x]](f: A => G[B])(implicit F: Applicative[G]): Resource[G, B] = {
      self.flatMap { a => f(a).toResource }
    }

    /**
     * Helps to decrease chance of getting StackOverflowError described in https://github.com/typelevel/cats-effect/issues/469
     */
    def breakFlatMapChain(implicit F: BracketThrowable[F]): Resource[F, A] = {
      Resource.suspend {
        self
          .allocated
          .map { a => Resource(a.pure[F]) }
      }
    }
  }


  class ResourceObjOpsCatsHelper(val self: Resource.type) extends AnyVal {

    def release[F[_] : Applicative](release: F[Unit]): Resource[F, Unit] = {
      Resource(((), release).pure[F])
    }
  }


  class BooleanOpsCatsHelper(val self: Boolean) extends AnyVal {

    def trueOr[A](a: => A): Either[A, Unit] = {
      if (self) ().asRight else a.asLeft
    }

    def falseOr[A](a: => A): Either[A, Unit] = {
      if (self) a.asLeft else ().asRight
    }

    def trueOrF[F[_]]: BooleanOpsTrueOrFApply[F] = {
      new BooleanOpsTrueOrFApply[F](self)
    }

    def falseOrF[F[_]]: BooleanOpsFalseOrFApply[F] = {
      new BooleanOpsFalseOrFApply[F](self)
    }
  }

  class BooleanOpsTrueOrFApply[F[_]](val b: Boolean) extends AnyVal {
    def apply[A](left: => A)(implicit F: Applicative[F]): EitherT[F, A, Unit] = {
      val either = if (b) Right(()) else Left(left)
      EitherT(either.pure[F])
    }
  }

  class BooleanOpsFalseOrFApply[F[_]](val b: Boolean) extends AnyVal {
    def apply[A](left: => A)(implicit F: Applicative[F]): EitherT[F, A, Unit] = {
      val either = if (b) Left(left) else Right(())
      EitherT(either.pure[F])
    }
  }
}
