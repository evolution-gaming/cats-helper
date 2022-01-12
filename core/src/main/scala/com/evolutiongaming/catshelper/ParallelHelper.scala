package com.evolutiongaming.catshelper

import cats.{Foldable, Monoid, Parallel}

object ParallelHelper extends ParallelSyntax

trait ParallelSyntax {

  import ParallelHelperOps._

  implicit def toParallelObjOps_ParallelHelper(self: Parallel.type): ParallelObjOps_ParallelHelper = new ParallelObjOps_ParallelHelper(self)

  implicit def toParallelOps_ParallelHelper[T[_], A](self: T[A]): ParallelOps_ParallelHelper[T, A] = new ParallelOps_ParallelHelper(self)

  implicit def toParallelFOps_ParallelHelper[T[_], F[_], A](self: T[F[A]]): ParallelFOps_ParallelHelper[T, F, A] = new ParallelFOps_ParallelHelper(self)

  implicit def toIterableOnce_ParallelHelper[A](self: TraversableOnce[A]): IterableOnce_ParallelHelper[A] = new IterableOnce_ParallelHelper(self)
}

private[catshelper] object ParallelHelperOps {

  implicit class ParallelObjOps_ParallelHelper(val self: Parallel.type) extends AnyVal {

    def parFoldMap[T[_] : Foldable, F[_], A, B: Monoid](ta: T[A])(f: A => F[B])(implicit P: Parallel[F]): F[B] = {
      val M = Monoid[B]
      val A = P.applicative
      val zero = A.pure(M.empty)
      val fa = Foldable[T].foldLeft(ta, zero) { case (b, a) =>
        A.map2(b, P.parallel(f(a)))(M.combine)
      }
      P.sequential(fa)
    }

    def parFold[T[_] : Foldable, F[_], A: Monoid](tfa: T[F[A]])(implicit P: Parallel[F]): F[A] = {
      parFoldMap(tfa)(Predef.identity)
    }

    def parFoldMapTraversable[F[_] : Parallel, A, B: Monoid](ta: TraversableOnce[A])(f: A => F[B]): F[B] = {
      val P = Parallel[F]
      val M = Monoid[B]
      val A = P.applicative
      val zero = A.pure(M.empty)
      val fb = ta.foldLeft(zero) { case (b, a) =>
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

  implicit class IterableOnce_ParallelHelper[A](val self: TraversableOnce[A]) extends AnyVal {
    def parFoldMapTraversable[F[_] : Parallel, B: Monoid](f: A => F[B]): F[B] = {
      Parallel.parFoldMapTraversable(self)(f)
    }
  }
}
