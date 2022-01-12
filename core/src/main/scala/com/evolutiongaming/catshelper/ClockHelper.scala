package com.evolutiongaming.catshelper

import cats.effect.Clock
import cats.implicits._
import cats.{Applicative, Functor}

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object ClockHelper {

  implicit class ClockOps[F[_]](val self: Clock[F]) extends AnyVal {

    def millis(implicit F: Functor[F]): F[Long] = self.realTime.map(_.toMillis)

    def nanos(implicit F: Functor[F]): F[Long] = self.monotonic.map(_.toNanos)

    def micros(implicit F: Functor[F]): F[Long] = self.monotonic.map(_.toMicros)

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

      def applicative: Applicative[F] = Applicative[F]

      def realTime: F[FiniteDuration] = FiniteDuration(millis, TimeUnit.MILLISECONDS).pure[F]

      def monotonic: F[FiniteDuration] = FiniteDuration(nanos, TimeUnit.NANOSECONDS).pure[F]
    }

    def empty[F[_] : Applicative]: Clock[F] = const(nanos = 0, millis = 0)
  }
}
