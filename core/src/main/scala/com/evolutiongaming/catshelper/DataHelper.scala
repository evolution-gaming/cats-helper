package com.evolutiongaming.catshelper

import cats.Order
import cats.data.{NonEmptyList => Nel, NonEmptyMap => Nem, NonEmptySet => Nes}

import scala.collection.immutable.{Iterable, SortedMap, SortedSet}

object DataHelper {

  implicit class SortedMapOpsDataHelper[K, V](val self: SortedMap[K, V]) extends AnyVal {

    @deprecated("no longer required by cats" , since = "2.1.1")
    def toNem(implicit order: Order[K]): Option[Nem[K, V]] = Nem.fromMap(self)

    def toNem(): Option[Nem[K, V]] = Nem.fromMap(self)
  }


  implicit class IterableOpsDataHelper[K, V](val self: Iterable[(K, V)]) extends AnyVal {

    def toSortedMap(implicit order: Order[K]): SortedMap[K, V] = {
      implicit val ordering = order.toOrdering
      val builder = SortedMap.newBuilder[K, V]
      builder ++= self
      builder.result()
    }

    def toNem(implicit order: Order[K]): Option[Nem[K, V]] = {
      self
        .toSortedMap
        .toNem()
    }
  }


  implicit class IterableOps1DataHelper[A](val self: Iterable[A]) extends AnyVal {

    def toSortedSet(implicit order: Order[A]): SortedSet[A] = {
      implicit val ordering = Order[A].toOrdering
      val builder = SortedSet.newBuilder[A]
      builder ++= self
      builder.result()
    }
  }


  implicit class NemOpsDataHelper[K, V](val self: Nem[K, V]) extends AnyVal {

    def mapKV[A: Order, B](f: (K, V) => (A, B)): Nem[A, B] = {
      implicit val ordering = Order[A].toOrdering
      self
        .toSortedMap
        .map { case (k, v) => f(k, v) }
        .toNem()
        .get
    }

    def mapK[A: Order](f: K => A): Nem[A, V] = {
      implicit val ordering = Order[A].toOrdering
      self
        .toSortedMap
        .map { case (k, v) => (f(k), v) }
        .toNem()
        .get
    }
  }


  implicit class NelOpsDataHelper[A](val self: Nel[A]) extends AnyVal {

    def grouped(n: Int): Nel[Nel[A]] = {
      if (n <= 0) Nel.of(self)
      else {
        val groups = self
          .toList
          .grouped(n)
          .toList
          .map { a => Nel(a.head, a.tail) }
        Nel(groups.head, groups.tail)
      }
    }
  }


  implicit class NesOpsDataHelper[A](val self: Nes[A]) extends AnyVal {

    def toNel: Nel[A] = self.toNonEmptyList
  }
}

