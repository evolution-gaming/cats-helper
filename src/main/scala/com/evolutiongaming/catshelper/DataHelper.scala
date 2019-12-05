package com.evolutiongaming.catshelper

import cats.Order
import cats.data.{NonEmptyMap => Nem, NonEmptyList => Nel}

import scala.collection.immutable.{Iterable, SortedMap}

object DataHelper {

  implicit class IterableOpsDataHelper[K, V](val self: Iterable[(K, V)]) extends AnyVal {

    def toSortedMap(implicit order: Order[K]): SortedMap[K, V] = {
      implicit val ordering = order.toOrdering
      val builder = SortedMap.newBuilder[K, V]
      val result = builder ++= self
      result.result()
    }

    def toNem(implicit order: Order[K]): Option[Nem[K, V]] = {
      val sortedMap = self.toSortedMap
      Nem.fromMap(sortedMap)
    }
  }


  implicit class NemOpsDataHelper[K, V](val self: Nem[K, V]) extends AnyVal {

    def mapKV[A: Order, B](f: (K, V) => (A, B)): Nem[A, B] = {
      implicit val ordering = Order[A].toOrdering
      self
        .toSortedMap
        .map { case (k, v) => f(k, v) }
        .toNem
        .get
    }

    def mapK[A: Order](f: K => A): Nem[A, V] = {
      implicit val ordering = Order[A].toOrdering
      self
        .toSortedMap
        .map { case (k, v) => (f(k), v) }
        .toNem
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
}

