package com.evolutiongaming.catshelper

import cats.effect.{Blocker, ContextShift}
import cats.~>

import scala.concurrent.ExecutionContext

trait Blocking[F[_]] {

  def apply[A](fa: F[A]): F[A]
}

object Blocking {

  def summon[F[_]](implicit F: Blocking[F]): Blocking[F] = F


  def empty[F[_]]: Blocking[F] = new Blocking[F] {
    def apply[A](fa: F[A]) = fa
  }


  def fromBlocker[F[_]: ContextShift](blocker: Blocker): Blocking[F] = new Blocking[F] {
    def apply[A](fa: F[A]) = blocker.blockOn(fa)
  }


  def fromExecutionContext[F[_]: ContextShift](executor: ExecutionContext): Blocking[F] = new Blocking[F] {
    def apply[A](fa: F[A]) = ContextShift[F].evalOn(executor)(fa)
  }


  object implicits {

    implicit class OpsBlocking[F[_], A](val self: F[A]) extends AnyVal {

      def blocking(implicit blocking: Blocking[F]): F[A] = blocking(self)
    }
  }


  implicit class BlockingOps[F[_]](val self: Blocking[F]) extends AnyVal {

    def mapK[G[_]](fg: F ~> G, gf: G ~> F): Blocking[G] = new Blocking[G] {
      def apply[A](fa: G[A]) = fg(self(gf(fa)))
    }
  }
}