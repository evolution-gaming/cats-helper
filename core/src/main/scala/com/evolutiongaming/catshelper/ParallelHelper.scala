package com.evolutiongaming.catshelper

import cats.{Foldable, Monoid, Parallel}

object ParallelHelper {

  implicit class ParallelObjOps_ParallelHelper(val self: Parallel.type) extends AnyVal {

    def parFoldMap[T[_]: Foldable, F[_], A, B: Monoid](ta: T[A])(f: A => F[B])(implicit P: Parallel[F]): F[B] = {
      val M = Monoid[B]
      val A = P.applicative
      val zero = A.pure(M.empty)
      val fa = Foldable[T].foldLeft(ta, zero) { case (b, a) =>
        A.map2(b, P.parallel(f(a)))(M.combine)
      }
      P.sequential(fa)
    }

    def parFold[T[_]: Foldable, F[_], A: Monoid](tfa: T[F[A]])(implicit P: Parallel[F]): F[A] = {
      parFoldMap(tfa)(Predef.identity)
    }

    def parFoldMapIterable[F[_]: Parallel, A, B: Monoid](ta: IterableOnce[A])(f: A => F[B]): F[B] = {
      val P = Parallel[F]
      val M = Monoid[B]
      val A = P.applicative
      val zero = A.pure(M.empty)
      val fb = ta
        .iterator
        .foldLeft(zero) { case (b, a) =>
          A.map2(b, P.parallel(f(a)))(M.combine)
        }
      P.sequential(fb)
    }
  }


  implicit class ParallelOps_ParallelHelper[T[_], A](val self: T[A]) extends AnyVal {

    def parFoldMap[F[_], B: Monoid](f: A => F[B])(implicit F: Foldable[T], P: Parallel[F]): F[B] = {
      Parallel.parFoldMap(self)(f)
    }
  }


  implicit class ParallelFOps_ParallelHelper[T[_], F[_], A](val self: T[F[A]]) extends AnyVal {

    def parFold(implicit M: Monoid[A], F: Foldable[T], P: Parallel[F]): F[A] = {
      Parallel.parFold(self)
    }
  }

  implicit class IterableOnce_ParallelHelper[A](val self: IterableOnce[A]) extends AnyVal {
    def parFoldMapIterable[F[_]: Parallel, B: Monoid](f: A => F[B]): F[B] = {
      Parallel.parFoldMapIterable(self)(f)
    }
  }
}
