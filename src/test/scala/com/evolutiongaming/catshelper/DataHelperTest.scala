package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel, NonEmptyMap => Nem, NonEmptySet => Nes}
import cats.implicits._
import com.evolutiongaming.catshelper.DataHelper._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.{SortedMap, SortedSet}

class DataHelperTest extends AnyFunSuite with Matchers {

  test("Iterable.toSortedMap") {
    List.empty[(Int, Int)].toSortedMap shouldEqual SortedMap.empty[Int, Int]
    Map((0, 0), (1, 1), (2, 2)).toSortedMap shouldEqual SortedMap((0, 0), (1, 1), (2, 2))
  }

  test("Iterable.toNem") {
    Map((0, 1)).toNem shouldEqual Nem.of((0, 1)).some
    Map.empty[Int, Int].toNem shouldEqual none
  }

  test("Nel.grouped") {
    val actual = Nel
      .of(0, 1, 2, 3, 4)
      .grouped(2)
    val expected = Nel.of(
      Nel.of(0, 1),
      Nel.of(2, 3),
      Nel.of(4))
    actual shouldEqual expected
  }

  test("Iterable.toSortedSet") {
    List.empty[Int].toSortedSet shouldEqual SortedSet.empty[Int]
    List(1, 0, 1).toSortedSet shouldEqual SortedSet(0, 1)
  }

  test("Iterable.toNes") {
    List.empty[Int].toNes shouldEqual none
    List(1, 0, 1).toNes shouldEqual Nes.of(0, 1).some
  }
}
