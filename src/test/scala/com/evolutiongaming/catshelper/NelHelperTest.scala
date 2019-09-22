package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import com.evolutiongaming.catshelper.NelHelper._
import org.scalatest.{FunSuite, Matchers}

class NelHelperTest extends FunSuite with Matchers {

  test("grouped") {
    val actual = Nel
      .of(0, 1, 2, 3, 4)
      .grouped(2)
    val expected = Nel.of(
      Nel.of(0, 1),
      Nel.of(2, 3),
      Nel.of(4))
    actual shouldEqual expected
  }
}
