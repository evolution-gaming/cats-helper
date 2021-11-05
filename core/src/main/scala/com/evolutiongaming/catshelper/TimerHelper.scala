package com.evolutiongaming.catshelper

import cats.effect.kernel.{Deferred, Fiber, Poll, Ref, Unique}
import cats.effect.{Clock, Concurrent, Temporal}
import cats.implicits._
import com.evolutiongaming.catshelper.ClockHelper._

import scala.concurrent.duration.FiniteDuration

object TimerHelper {

  implicit class TimerObjOpsTimerHelper(val self: Temporal.type) extends AnyVal {

    def empty[F[_]](implicit F: Concurrent[F]): Temporal[F] = new Temporal[F] {

      val clock = Clock.empty[F]

      def pure[A](x: A): F[A] = F.pure(x)

      def raiseError[A](e: Throwable): F[A] = F.raiseError(e)

      def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)

      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => F[Either[A,B]]): F[B] = F.tailRecM(a)(f)

      def forceR[A, B](fa: F[A])(fb: F[B]): F[B] = F.forceR(fa)(fb)

      def uncancelable[A](body: Poll[F] => F[A]): F[A] = F.uncancelable(body)

      def canceled: F[Unit] = F.canceled

      def onCancel[A](fa: F[A], fin: F[Unit]): F[A] = F.onCancel(fa, fin)

      def unique: F[Unique.Token] = F.unique

      def start[A](fa: F[A]): F[Fiber[F,Throwable,A]] = F.start(fa)

      def never[A]: F[A] = F.never

      def cede: F[Unit] = F.cede

      def ref[A](a: A): F[Ref[F,A]] = F.ref(a)

      def deferred[A]: F[Deferred[F,A]] = F.deferred

      def monotonic: F[FiniteDuration] = clock.monotonic

      def realTime: F[FiniteDuration] = clock.realTime

      def sleep(duration: FiniteDuration): F[Unit] = ().pure[F]
    }
  }
}
