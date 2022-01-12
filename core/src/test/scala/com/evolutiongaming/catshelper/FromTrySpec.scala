package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.implicits._
import com.evolutiongaming.catshelper.CatsHelper._

import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FromTrySpec extends AnyFunSuite with Matchers {

  def failure[A](a: Throwable): Try[A] = Failure(a)

  def success[A](a: A): Try[A] = Success(a)

  for {
    (name, value, expected) <- List(
      ("success", success(())         , ().pure[IO]),
      ("failure", failure[Unit](Error), Error.raiseError[IO, Unit])
    )
  } {
    test(name) {
      value.fromTry[IO] shouldEqual expected
    }
  }


  private case object Error extends RuntimeException with NoStackTrace
}