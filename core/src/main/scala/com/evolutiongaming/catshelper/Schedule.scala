package com.evolutiongaming.catshelper

import cats.effect.implicits._
import cats.effect.{Concurrent, Resource}
import cats.implicits._

import scala.concurrent.duration.FiniteDuration
import cats.effect.Temporal

object Schedule {

  def apply[F[_]: Concurrent: Temporal](
    initial: FiniteDuration,
    interval: FiniteDuration)(
    fa: F[Unit]
  ): Resource[F, Unit] = {

    val schedule = for {
      _ <- fa
      _ <- Temporal[F].sleep(interval)
    } yield {}

    val daemon = for {
      _ <- Temporal[F].sleep(initial)
      _ <- schedule.foreverM[Unit]
    } yield {}

    Resource
      .make { daemon.start } { _.cancel }
      .void
  }
}