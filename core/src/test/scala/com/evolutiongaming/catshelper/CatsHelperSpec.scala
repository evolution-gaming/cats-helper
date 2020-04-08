package com.evolutiongaming.catshelper

import cats.implicits._
import com.evolutiongaming.catshelper.CatsHelper._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Try}

class CatsHelperSpec extends AnyFunSuite with Matchers {

  test("castM") {
    val a: Any = ""
    a.castM[Try, String] shouldEqual "".pure[Try]
    a.castM[Try, Int] should matchPattern { case Failure(_: ClassCastException) => }
  }

  test("castOpt") {
    val a: Any = ""
    a.castOpt[String] shouldEqual "".some
    a.castOpt[Int] shouldEqual none
  }
}
