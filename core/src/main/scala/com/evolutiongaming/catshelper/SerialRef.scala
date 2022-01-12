package com.evolutiongaming.catshelper

import cats.effect._
import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.implicits._
import cats.~>

trait SerialRef[F[_], A] {

  def get: F[A]

  def modify[B](f: A => F[(A, B)]): F[B]

  def update(f: A => F[A]): F[Unit]
}

object SerialRef { self =>

  def apply[F[_]](implicit F: Concurrent[F]): Apply[F] = new Apply(F)


  def of[F[_] : Concurrent, A](value: A): F[SerialRef[F, A]] = {
    for {
      s <- Semaphore[F](1)
      r <- Ref[F].of(value)
    } yield {
      new SerialRef[F, A] {

        val get = r.get

        def modify[B](f: A => F[(A, B)]) = {
          s.permit.use { _ =>
            for {
              a      <- r.get
              ab     <- f(a)
              (a, b)  = ab
              _      <- r.set(a)
            } yield b
          }
        }

        def update(f: A => F[A]) = {
          modify { a =>
            for {
              a <- f(a)
            } yield {
              (a, ())
            }
          }
        }
      }
    }
  }


  class Apply[F[_]](val F: Concurrent[F]) extends AnyVal {
    
    def of[A](value: A): F[SerialRef[F, A]] = self.of[F, A](value)(F)
  }


  implicit class SerialRefOps[F[_], A](val self: SerialRef[F, A]) extends AnyVal {

    def mapK[G[_]](to: F ~> G, from: G ~> F): SerialRef[G, A] = new SerialRef[G, A] {

      val get = to(self.get)

      def modify[B](f: A => G[(A, B)]) = to(self.modify(a => from(f(a))))

      def update(f: A => G[A]) = to(self.update(a => from(f(a))))
    }
  }
}