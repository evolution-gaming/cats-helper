package com.evolution.catshelper

import cats.effect.Sync
import cats.syntax.all._
import cats.{Applicative, ~>}

import java.lang.{Runtime => RuntimeJ}

trait Runtime[F[_]] {

  def availableCores: F[Int]

  def freeMemory: F[Long]

  def totalMemory: F[Long]

  def maxMemory: F[Long]

  def gc: F[Unit]
}

object Runtime {

  def apply[F[_]](implicit F: Runtime[F]): Runtime[F] = F

  def summon[F[_]](implicit F: Runtime[F]): Runtime[F] = F

  def empty[F[_]: Applicative]: Runtime[F] = new Runtime[F] {

    def availableCores = 0.pure[F]

    def freeMemory = 0L.pure[F]

    def totalMemory = 0L.pure[F]

    def maxMemory = 0L.pure[F]

    def gc = ().pure[F]
  }


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

      val availableCores = f(self.availableCores)

      val freeMemory = f(self.freeMemory)

      val totalMemory = f(self.totalMemory)

      val maxMemory = f(self.maxMemory)

      val gc = f(self.gc)
    }
  }
}