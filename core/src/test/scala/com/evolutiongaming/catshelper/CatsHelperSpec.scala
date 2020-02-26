package com.evolutiongaming.catshelper

import cats.effect._
import cats.implicits._
import com.evolutiongaming.catshelper.CatsHelper._

import scala.util.control.NoStackTrace
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CatsHelperSpec extends AnyFunSuite with Matchers {

  for {
    (name, a, expected) <- List(
      ("pure", 0.pure[IO], "0"),
      ("error", TestError.raiseError[IO, Unit], "TestError"))
  } {
    test(s"redeem $name") {
      redeem(a).unsafeRunSync() shouldEqual expected
    }

    test(s"redeemWith $name") {
      redeemWith(a).unsafeRunSync() shouldEqual expected
    }
  }

  private def redeem[F[_], A, E](a: F[A])(implicit bracket: Bracket[F, E]) = {
    a.redeem[String, E](_.toString, _.toString)
  }

  private def redeemWith[F[_], A, E](a: F[A])(implicit bracket: Bracket[F, E]) = {
    a.redeemWith[String, E](_.toString.pure[F], _.toString.pure[F])
  }

  case object TestError extends RuntimeException with NoStackTrace {
    override def toString: String = "TestError"
  }
}
