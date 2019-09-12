package com.evolutiongaming.catshelper

import cats.{Applicative, Foldable, Monoid, Parallel}

object ParallelHelper {

  implicit class ParallelObjOps_ParallelHelper(val self: Parallel.type) extends AnyVal {

    def parFoldMap[T[_] : Foldable, F[_], A, B: Monoid](ta: T[A])(f: A => F[B])(implicit P: Parallel[F]): F[B] = {
      val applicative = P.applicative
      implicit val monoid = Applicative.monoid[P.F, B](applicative, Monoid[B])
      val fb = Foldable[T].foldMap(ta)(f.andThen(P.parallel.apply(_)))
      P.sequential(fb)
    }

    def parFold[T[_] : Foldable, F[_], A: Monoid](tfa: T[F[A]])(implicit P: Parallel[F]): F[A] = {
      parFoldMap(tfa)(Predef.identity)
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
}
