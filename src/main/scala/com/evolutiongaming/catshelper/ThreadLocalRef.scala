package com.evolutiongaming.catshelper

import cats.effect.{IO, Sync}
import cats.~>

trait ThreadLocalRef[F[_], A] {

  def get: F[A]

  def set(a: A): F[Unit]

  def update(f: A => A): F[Unit]

  def modify[B](f: A => (A, B)): F[B]
}

object ThreadLocalRef {

  def apply[F[_] : Sync, A](threadLocal: ThreadLocal[A]): ThreadLocalRef[F, A] = new ThreadLocalRef[F, A] {

    def get = Sync[F].delay { threadLocal.get() }

    def set(a: A) = Sync[F].delay { threadLocal.set(a) }

    def update(f: A => A) = {
      Sync[F].delay {
        val a0 = threadLocal.get()
        val a = f(a0)
        threadLocal.set(a)
      }
    }

    def modify[B](f: A => (A, B)) = {
      Sync[F].delay {
        val a0 = threadLocal.get()
        val (a, b) = f(a0)
        threadLocal.set(a)
        b
      }
    }
  }


  implicit class ThreadLocalRefOps[F[_], A](val self: ThreadLocalRef[F, A]) extends AnyVal {

    def mapK[G[_]](f: F ~> G): ThreadLocalRef[G, A] = new ThreadLocalRef[G, A] {

      def get = f(self.get)

      def set(a: A) = f(self.set(a))

      def update(f1: A => A) = f(self.update(f1))

      def modify[B](f1: A => (A, B)) = f(self.modify(f1))
    }
  }
}


trait ThreadLocalOf[F[_]] {

  def apply[A](fa: F[A]): F[ThreadLocalRef[F, A]]
}

object ThreadLocalOf {

  def apply[F[_]](implicit F: ThreadLocalOf[F]): ThreadLocalOf[F] = F

  implicit val ioThreadLocalOf: ThreadLocalOf[IO] = new ThreadLocalOf[IO] {

    def apply[A](fa: IO[A]) = {
      val threadLocal = Sync[IO].delay {
        new ThreadLocal[A] { override def initialValue() = fa.unsafeRunSync() }
      }
      for {
        threadLocal <- threadLocal
      } yield {
        ThreadLocalRef[IO, A](threadLocal)
      }
    }
  }

  @deprecated("use ioThreadLocalOf instead", "0.0.26")
  val io: ThreadLocalOf[IO] = ioThreadLocalOf
}
