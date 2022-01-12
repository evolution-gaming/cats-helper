package com.evolutiongaming.catshelper

import cats.effect.implicits._
import cats.effect.{Resource, Temporal}
import cats.implicits._

import scala.concurrent.duration.FiniteDuration

object Schedule {

  def apply[F[_]: Temporal](
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