package com.evolutiongaming.catshelper

import cats.Foldable
import cats.data.{NonEmptyList => Nel, NonEmptySet => Nes}

import scala.collection.immutable.Iterable

trait Foldable1[F[_]] {
  def fold[A, B](fa: F[A], b: B)(f: (B, A) => B): B
}

object Foldable1 {
  implicit val iterableFoldable1: Foldable1[Iterable] = new Foldable1[Iterable] {
    def fold[A, B](fa: Iterable[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)
  }

  implicit val iteratorFoldable1: Foldable1[Iterator] = new Foldable1[Iterator] {
    def fold[A, B](fa: Iterator[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)
  }

  implicit val listFoldable1: Foldable1[List] = new Foldable1[List] {
    def fold[A, B](fa: List[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)
  }

  implicit val nelFoldable1: Foldable1[Nel] = new Foldable1[Nel] {
    def fold[A, B](fa: Nel[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)
  }

  implicit val nesFoldable1: Foldable1[Nes] = new Foldable1[Nes] {
    def fold[A, B](fa: Nes[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)
  }

  implicit val vectorFoldable1: Foldable1[Vector] = new Foldable1[Vector] {
    def fold[A, B](fa: Vector[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)
  }

  implicit val seqFoldable1: Foldable1[Seq] = new Foldable1[Seq] {
    def fold[A, B](fa: Seq[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)
  }

  def fromFoldable[F[_]](foldable1: Foldable[F]): Foldable1[F] = new Foldable1[F] {
    def fold[A, B](fa: F[A], b: B)(f: (B, A) => B) = foldable1.foldLeft(fa, b)(f)
  }

  object implicits {
    implicit class OpsFoldable1[F[_], A](val fa: F[A]) extends AnyVal {
      def fold[B](b: B)(f: (B, A) => B)(implicit F: Foldable1[F]): B = F.fold(fa, b)(f)
    }
  }
}
