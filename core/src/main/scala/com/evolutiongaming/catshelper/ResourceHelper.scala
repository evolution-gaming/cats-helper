package com.evolutiongaming.catshelper

import cats.implicits.*
import cats.effect.{Resource, Sync}

object ResourceHelper {
  implicit final class IOAppResourceOps[F[_], A](val self: Resource[F, A]) extends AnyVal {
    def log(name: String)(implicit F: Sync[F], log: Log[F], md: MeasureDuration[F]): Resource[F, A] = Resource {
      for {
        timedAcquireAndRelease <- for {
          getMeasurement           <- MeasureDuration[F].start
          _                        <- Log[F].info(s"$name acquiring")
          a                        <- self.allocated.attemptTap {
            case Left(err) => for {
              measureResult <- getMeasurement
              _ <- Log[F].error(s"$name acquisition failed in ${measureResult.toMillis}ms with $err", err)
            } yield ()

            case Right(_) => for {
              measureResult <- getMeasurement
              _ <- Log[F].info(s"$name acquired in ${measureResult.toMillis}ms")
            } yield ()
          }
        } yield a

        (timedAcquire, release) = timedAcquireAndRelease

        timedRelease = for {
          getMeasurement <- MeasureDuration[F].start
          _              <- Log[F].info(s"$name releasing")
          _ <- release.attemptTap {
            case Left(err) => for {
              measureResult <- getMeasurement
              _ <- Log[F].error(s"$name release failed in ${measureResult.toMillis}ms with $err", err)
            } yield ()

            case Right(_) => for {
              measureResult <- getMeasurement
              _             <- Log[F].info(s"$name released in ${measureResult.toMillis}ms")
            } yield ()
          }
        } yield ()

      } yield (timedAcquire, timedRelease)
    }
  }
}