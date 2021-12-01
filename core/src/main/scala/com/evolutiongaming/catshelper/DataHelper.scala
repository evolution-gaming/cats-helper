package com.evolutiongaming.catshelper

import cats.Order
import cats.data.{NonEmptyList => Nel, NonEmptyMap => Nem, NonEmptySet => Nes}

import scala.collection.immutable.{Iterable, SortedMap, SortedSet}

object DataHelper extends DataSyntax

trait DataSyntax {

  import DataHelperOps._


  implicit def toSortedMapOpsDataHelper[K, V](self: SortedMap[K, V]): SortedMapOpsDataHelper[K, V] = new SortedMapOpsDataHelper(self)

  implicit def toIterableOpsDataHelper[K, V](self: Iterable[(K, V)]): IterableOpsDataHelper[K, V] = new IterableOpsDataHelper(self)

  implicit def toIterableOps1DataHelper[A](self: Iterable[A]): IterableOps1DataHelper[A] = new IterableOps1DataHelper(self)

  implicit def toNemOpsDataHelper[K, V](self: Nem[K, V]): NemOpsDataHelper[K, V] = new NemOpsDataHelper(self)

  implicit def toNelOpsDataHelper[A](self: Nel[A]): NelOpsDataHelper[A] = new NelOpsDataHelper(self)

  implicit def toNesOpsDataHelper[A](self: Nes[A]): NesOpsDataHelper[A] = new NesOpsDataHelper(self)

}

private[catshelper] object DataHelperOps {

  implicit class SortedMapOpsDataHelper[K, V](val self: SortedMap[K, V]) extends AnyVal {

    @deprecated("no longer required by cats", since = "2.1.1")
    def toNem(implicit order: Order[K]): Option[Nem[K, V]] = Nem.fromMap(self)

    def toNem(): Option[Nem[K, V]] = Nem.fromMap(self)
  }


  implicit class IterableOpsDataHelper[K, V](val self: Iterable[(K, V)]) extends AnyVal {

    def toSortedMap(implicit order: Order[K]): SortedMap[K, V] = {
      implicit val ordering: Ordering[K] = order.toOrdering
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
      implicit val ordering: Ordering[A] = Order[A].toOrdering
      val builder = SortedSet.newBuilder[A]
      builder ++= self
      builder.result()
    }
  }


  implicit class NemOpsDataHelper[K, V](val self: Nem[K, V]) extends AnyVal {

    def mapKV[A: Order, B](f: (K, V) => (A, B)): Nem[A, B] = {
      implicit val ordering: Ordering[A] = Order[A].toOrdering
      self
        .toSortedMap
        .map { case (k, v) => f(k, v) }
        .toNem()
        .get
    }

    def mapK[A: Order](f: K => A): Nem[A, V] = {
      implicit val ordering: Ordering[A] = Order[A].toOrdering
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

    /**
     * Alias for [[grouped()]] to avoid name clashing with
     * [[cats.data.NonEmptyList.grouped]].
     */
    def groupedNel(n: Int): Nel[Nel[A]] = grouped(n)
  }


  implicit class NesOpsDataHelper[A](val self: Nes[A]) extends AnyVal {

    def toNel: Nel[A] = self.toNonEmptyList
  }
}

