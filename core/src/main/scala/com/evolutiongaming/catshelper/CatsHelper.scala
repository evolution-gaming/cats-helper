package com.evolutiongaming.catshelper

import cats.data.EitherT
import cats.effect.implicits.*
import cats.effect.kernel.{Deferred, Fiber}
import cats.effect.{Concurrent, Resource, Sync}
import cats.implicits.*
import cats.{Applicative, ApplicativeError, MonadError}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag
import scala.util.{Either, Try}

object CatsHelper {

  class ApplicativeErrorOpsCatsHelper[F[_], E](val self: ApplicativeError[F, E]) extends AnyVal {

    def redeem[A, B](fa: F[A])(recover: E => B, ab: A => B): F[B] = {
      val fb = self.map(fa)(ab)
      self.handleError(fb)(recover)
    }
  }


  implicit class MonadErrorOpsCatsHelper[F[_], E](val self: MonadError[F, E]) extends AnyVal {

    def redeemWith[A, B](fa: F[A])(recover: E => F[B], ab: A => F[B]): F[B] = {
      val fb = self.flatMap(fa)(ab)
      self.handleErrorWith(fb)(recover)
    }
  }


  implicit class ConcurrentOpsCatsHelper[F[_]](val self: Concurrent[F]) extends AnyVal {

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


  implicit class IdOpsCatsHelper[A](val self: A) extends AnyVal {

    def castM[F[_]: MonadThrowable, B <: A](implicit tag: ClassTag[B]): F[B] = {

      def error = new ClassCastException(s"${ self.getClass.getName } cannot be cast to ${ tag.runtimeClass.getName }")

      castOpt[B].fold { error.raiseError[F, B] } { _.pure[F] }
    }


    def castOpt[B <: A](implicit tag: ClassTag[B]): Option[B] = tag.unapply(self)
  }


  implicit class OpsCatsHelper[F[_], A](val self: F[A]) extends AnyVal {

    def startEnsure(implicit F: Concurrent[F]): F[Fiber[F, Throwable, A]] = F.startEnsure(self)


    def toTry(implicit F: ToTry[F]): Try[A] = F.apply(self)


    def toFuture(implicit F: ToFuture[F]): Future[A] = F.apply(self)
  }


  implicit class TryOpsCatsHelper[A](val self: Try[A]) extends AnyVal {

    def fromTry[F[_]](implicit fromTry: FromTry[F]): F[A] = fromTry(self)
  }


  implicit class ResourceOpsCatsHelper[F[_], A](val self: Resource[F, A]) extends AnyVal {

    def fenced(implicit F: Concurrent[F]): Resource[F, A] = ResourceFenced(self)

    def semiflatMap[B](f: A => F[B]): Resource[F, B] = {
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


  implicit class ResourceObjOpsCatsHelper(val self: Resource.type) extends AnyVal {

    def release[F[_]: Applicative](release: F[Unit]): Resource[F, Unit] = {
      Resource(((), release).pure[F])
    }
  }


  implicit class BooleanOpsCatsHelper(val self: Boolean) extends AnyVal {

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

  private[CatsHelper] class BooleanOpsTrueOrFApply[F[_]](val b: Boolean) extends AnyVal {
    def apply[A](left: => A)(implicit F: Applicative[F]): EitherT[F, A, Unit] = {
      val either = if (b) Right(()) else Left(left)
      EitherT(either.pure[F])
    }
  }

  private[CatsHelper] class BooleanOpsFalseOrFApply[F[_]](val b: Boolean) extends AnyVal {
    def apply[A](left: => A)(implicit F: Applicative[F]): EitherT[F, A, Unit] = {
      val either = if (b) Left(left) else Right(())
      EitherT(either.pure[F])
    }
  }

  implicit final class ResourceObservabilityOps[F[_], A](val self: Resource[F, A]) extends AnyVal {

    /**
     * Add callbacks to a resource. Use for things like metrics and logs.
     * It's very important that these callbacks shouldn't fail,
     * otherwise there can be a resource leak.
     */
    def observe(
      onAcquireStart: Option[F[Unit]] = None,
      onAcquireSuccess: Option[(A, FiniteDuration) => F[Unit]] = None,
      onAcquireFail: Option[(Throwable, FiniteDuration) => F[Unit]] = None,
      onReleaseStart: Option[A => F[Unit]] = None,
      onReleaseSuccess: Option[FiniteDuration => F[Unit]] = None,
      onReleaseFail: Option[(Throwable, FiniteDuration) => F[Unit]] = None,
    )(implicit F: Sync[F], md: MeasureDuration[F]): Resource[F, A] = Resource {
      for {
        timedAcquireAndRelease <- for {
          getMeasurement <- MeasureDuration[F].start
          _ <- onAcquireStart.sequence_
          a <- self.allocated.attemptTap {
            case Left(err) => for {
              measureResult <- getMeasurement
              _ <- onAcquireFail.traverse_(_(err, measureResult))
            } yield ()

            case Right((obj, _)) => for {
              measureResult <- getMeasurement
              _ <- onAcquireSuccess.traverse_(_(obj, measureResult))
            } yield ()
          }
        } yield a

        (timedAcquire, release) = timedAcquireAndRelease

        timedRelease = for {
          getMeasurement <- MeasureDuration[F].start
          _ <- onReleaseStart.traverse_(_(timedAcquire))
          _ <- release.attemptTap {
            case Left(err) => for {
              measureResult <- getMeasurement
              _ <- onReleaseFail.traverse_(_(err, measureResult))
            } yield ()

            case Right(a) => for {
              measureResult <- getMeasurement
              _ <- onReleaseSuccess.traverse_(_(measureResult))
            } yield a
          }
        } yield ()

      } yield (timedAcquire, timedRelease)
    }
  }

  implicit final class ResourceLogOps[F[_], A](val self: Resource[F, A]) extends AnyVal {

    def log(name: String)(implicit F: Sync[F], log: Log[F], md: MeasureDuration[F]): Resource[F, A] = {
      self.observe(
        onAcquireStart = Some(
          Log[F].info(s"$name acquiring")
        ),
        onAcquireSuccess = Some((_, duration) =>
          Log[F].info(s"$name acquired in ${duration.toMillis}ms")
        ),
        onReleaseStart = Some(_ =>
          Log[F].info(s"$name releasing")
        ),
        onReleaseSuccess = Some(duration =>
          Log[F].info(s"$name released in ${duration.toMillis}ms")
        ),
        onAcquireFail = Some((err, duration) =>
          Log[F].error(s"$name acquisition failed in ${duration.toMillis}ms with $err", err)
        ),
        onReleaseFail = Some((err, duration) =>
          Log[F].error(s"$name release failed in ${duration.toMillis}ms with $err", err)
        ),
      )
    }
  }
}
