package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.implicits._
import com.evolutiongaming.catshelper.IOSuite._
import com.evolutiongaming.catshelper.EffectHelper._
import org.scalatest.{FunSuite, Matchers}

import scala.util.{Failure, Success, Try}
import scala.util.control.NoStackTrace

class ToTrySpec extends FunSuite with Matchers {

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