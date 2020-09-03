package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.syntax.all._
import com.evolutiongaming.catshelper.IOSuite._
import com.evolutiongaming.catshelper.CatsHelper._

import scala.util.{Failure, Success, Try}
import scala.util.control.NoStackTrace
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ToTrySpec extends AnyFunSuite with Matchers {

  def failure[A](a: Throwable): Try[A] = Failure(a)

  def success[A](a: A): Try[A] = Success(a)

  for {
    (name, value, expected) <- List(
      ("success", ().pure[IO],                success(())),
      ("failure", Error.raiseError[IO, Unit], failure[Unit](Error))
    )
  } {
    test(name) {
      value.toTry shouldEqual expected
    }
  }


  private case object Error extends RuntimeException with NoStackTrace
}