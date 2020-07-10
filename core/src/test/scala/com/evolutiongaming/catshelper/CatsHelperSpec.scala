package com.evolutiongaming.catshelper

import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
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

  test("toResource") {
    "".pure[IO].toResource.use { _.pure[IO] }.toTry shouldEqual "".pure[Try]
  }

  test("Resource.release") {
    val result = for {
      r <- Ref[IO].of(false)
      _ <- Resource.release(r.set(true)).use { _ => ().pure[IO] }
      a <- r.get
    } yield a
    result.toTry.get shouldEqual true
  }
}
