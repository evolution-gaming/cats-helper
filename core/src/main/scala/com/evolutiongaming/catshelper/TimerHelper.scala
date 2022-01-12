package com.evolutiongaming.catshelper

import cats.Applicative
import cats.effect.{Clock, Timer}
import cats.implicits._
import com.evolutiongaming.catshelper.ClockHelper._

import scala.concurrent.duration.FiniteDuration

trait TimerSyntax

object TimerHelper extends TimerSyntax {

  import TimerOps._

  implicit def toTimerObjOpsTimerHelper(self: Timer.type): TimerObjOpsTimerHelper = new TimerObjOpsTimerHelper(self)

}


private[catshelper] object TimerOps {

  implicit class TimerObjOpsTimerHelper(val self: Timer.type) extends AnyVal {

    def empty[F[_] : Applicative]: Timer[F] = new Timer[F] {

      val clock = Clock.empty[F]

      def sleep(duration: FiniteDuration) = ().pure[F]
    }
  }
}
