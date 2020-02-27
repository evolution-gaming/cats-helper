package com.evolutiongaming.catshelper

import java.time.Instant
import java.util.concurrent.TimeUnit

import cats.effect.Clock
import cats.implicits._
import cats.{Applicative, Functor}

object ClockHelper {

  implicit class ClockOps[F[_]](val self: Clock[F]) extends AnyVal {

    def millis: F[Long] = self.realTime(TimeUnit.MILLISECONDS)

    def nanos: F[Long] = self.monotonic(TimeUnit.NANOSECONDS)

    def micros: F[Long] = self.monotonic(TimeUnit.MICROSECONDS)

    def instant(implicit F: Functor[F]): F[Instant] = {
      for {
        millis <- millis
      } yield {
        Instant.ofEpochMilli(millis)
      }
    }
  }


  implicit class ClockObjOps(val self: Clock.type) extends AnyVal {

    def const[F[_] : Applicative](nanos: Long, millis: Long): Clock[F] = new Clock[F] {

      def realTime(unit: TimeUnit) = unit.convert(millis, TimeUnit.MILLISECONDS).pure[F]

      def monotonic(unit: TimeUnit) = unit.convert(nanos, TimeUnit.NANOSECONDS).pure[F]
    }

    def empty[F[_] : Applicative]: Clock[F] = const(nanos = 0, millis = 0)
  }
}
