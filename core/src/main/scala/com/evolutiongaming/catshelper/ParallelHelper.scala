package com.evolutiongaming.catshelper

import cats.{Foldable, Monoid, Parallel}
import cats.data.{NonEmptyMap => Nem}
import com.evolutiongaming.catshelper.Foldable1.implicits._

object ParallelHelper {

  implicit class ParallelObjOps_ParallelHelper(val self: Parallel.type) extends AnyVal {

    @deprecated("use `parFoldMap1` instead", "2.8.0")
    def parFoldMap[T[_]: Foldable, F[_], A, B: Monoid](ta: T[A])(f: A => F[B])(implicit P: Parallel[F]): F[B] = {
      val M = Monoid[B]
      val A = P.applicative
      val zero = A.pure(M.empty)
      val fb = Foldable[T].foldLeft(ta, zero) { case (b, a) =>
        A.map2(b, P.parallel(f(a)))(M.combine)
      }
      P.sequential(fb)
    }

    @deprecated("use `parFold1` instead", "2.8.0")
    def parFold[T[_]: Foldable, F[_], A: Monoid](tfa: T[F[A]])(implicit P: Parallel[F]): F[A] = {
      parFoldMap(tfa)(Predef.identity)
    }

    @deprecated("use `parFoldMap1` instead", "2.8.0")
    def parFoldMapTraversable[F[_]: Parallel, A, B: Monoid](ta: TraversableOnce[A])(f: A => F[B]): F[B] = {
      val P = Parallel[F]
      val M = Monoid[B]
      val A = P.applicative
      val zero = A.pure(M.empty)
      val fb = ta.foldLeft(zero) { case (b, a) =>
        A.map2(b, P.parallel(f(a)))(M.combine)
      }
      P.sequential(fb)
    }

    def parFoldMap1[F[_], P[_], A, B](
      fa: F[A])(
      f: A => P[B])(implicit
      F: Foldable1[F],
      P: Parallel[P],
      M: Monoid[B]
    ): P[B] = {
      val A = P.applicative
      val pb = fa.fold(A.pure(M.empty)) { case (pb, a) =>
        val fa = f(a)
        val pa = P.parallel(fa)
        A.map2(pb, pa)(M.combine)
      }
      P.sequential(pb)
    }

    def parFold1[F[_], P[_], A](
      fa: F[P[A]])(implicit
      F: Foldable1[F],
      P: Parallel[P],
      M: Monoid[A]
    ): P[A] = {
      val A = P.applicative
      val pb = fa.fold(A.pure(M.empty)) { case (b, a) =>
        A.map2(b, P.parallel(a))(M.combine)
      }
      P.sequential(pb)
    }
  }


  implicit class ParallelOps_ParallelHelper[T[_], A](val self: T[A]) extends AnyVal {

    @deprecated("use `parFoldMap1` instead", "2.8.0")
    def parFoldMap[F[_], B: Monoid](f: A => F[B])(implicit F: Foldable[T], P: Parallel[F]): F[B] = {
      Parallel.parFoldMap(self)(f)
    }
  }


  implicit class ParallelFOps_ParallelHelper[T[_], F[_], A](val self: T[F[A]]) extends AnyVal {

    @deprecated("use `parFold1` instead", "2.8.0")
    def parFold(implicit M: Monoid[A], F: Foldable[T], P: Parallel[F]): F[A] = {
      Parallel.parFold(self)
    }
  }

  implicit class IterableOnce_ParallelHelper[A](val self: TraversableOnce[A]) extends AnyVal {

    @deprecated("use `parFoldMap1` instead", "2.8.0")
    def parFoldMapTraversable[F[_]: Parallel, B: Monoid](f: A => F[B]): F[B] = {
      Parallel.parFoldMapTraversable(self)(f)
    }
  }

  implicit class OpsParallelHelper[F[_], A](val self: F[A]) extends AnyVal {

    def parFoldMap1[P[_]: Parallel, B: Monoid](f: A => P[B])(implicit F: Foldable1[F]): P[B] = {
      Parallel.parFoldMap1(self)(f)
    }
  }

  implicit class POpsParallelHelper[F[_], P[_], A](val self: F[P[A]]) extends AnyVal {

    def parFold1(implicit M: Monoid[A], F: Foldable1[F], P: Parallel[P]): P[A] = {
      Parallel.parFold1(self)
    }
  }

  implicit class MapOpsParallelHelper[K, V](val self: Map[K, V]) extends AnyVal {

    def parFoldMap1[P[_]: Parallel, B: Monoid](f: (K, V) => P[B]): P[B] = {
      self
        .iterator
        .parFoldMap1 { case (k, v) => f(k, v) }
    }
  }

  implicit class NemOpsParallelHelper[K, V](val self: Nem[K, V]) extends AnyVal {

    def parFoldMap1[P[_]: Parallel, B: Monoid](f: (K, V) => P[B]): P[B] = {
      self
        .toSortedMap
        .parFoldMap1(f)
    }
  }
}
