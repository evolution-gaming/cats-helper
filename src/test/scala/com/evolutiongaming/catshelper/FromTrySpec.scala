package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.implicits._
import com.evolutiongaming.catshelper.EffectHelper._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.{FunSuite, Matchers}

import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

class FromTrySpec extends FunSuite with Matchers {

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