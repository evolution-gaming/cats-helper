package com.evolutiongaming.catshelper

import java.lang.{Runtime => RuntimeJ}

import cats.effect.Sync
import cats.~>

trait Runtime[F[_]] {

  def availableCores: F[Int]

  def freeMemory: F[Long]

  def totalMemory: F[Long]

  def maxMemory: F[Long]

  def gc: F[Unit]
}

object Runtime {

  def apply[F[_]](implicit F: Runtime[F]): Runtime[F] = F

  def apply[F[_] : Sync](runtime: RuntimeJ): Runtime[F] = new Runtime[F] {

    val availableCores = Sync[F].delay { runtime.availableProcessors() }

    val freeMemory = Sync[F].delay { runtime.freeMemory() }

    val totalMemory = Sync[F].delay { runtime.totalMemory() }

    val maxMemory = Sync[F].delay { runtime.maxMemory() }

    val gc = Sync[F].delay { runtime.gc() }
  }

  implicit def lift[F[_] : Sync]: Runtime[F] = apply(RuntimeJ.getRuntime)


  implicit class RuntimeOps[F[_]](val self: Runtime[F]) extends AnyVal {

    def mapK[G[_]](f: F ~> G): Runtime[G] = new Runtime[G] {

      def availableCores = f(self.availableCores)

      def freeMemory = f(self.freeMemory)

      def totalMemory = f(self.totalMemory)

      def maxMemory = f(self.maxMemory)

      def gc = f(self.gc)
    }
  }
}