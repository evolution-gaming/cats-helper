package com.evolutiongaming.catshelper

import cats.Applicative
import cats.effect.Clock
import cats.implicits._
import com.evolutiongaming.catshelper.ClockHelper._

import scala.concurrent.duration.FiniteDuration
import cats.effect.Temporal

object TimerHelper {

  implicit class TimerObjOpsTimerHelper(val self: Temporal.type) extends AnyVal {

    def empty[F[_] : Applicative]: Temporal[F] = new Temporal[F] {

      val clock = Clock.empty[F]

      def sleep(duration: FiniteDuration) = ().pure[F]
    }
  }
}
