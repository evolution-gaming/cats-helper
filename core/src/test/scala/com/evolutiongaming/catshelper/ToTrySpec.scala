package com.evolutiongaming.catshelper

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.evolutiongaming.catshelper.IOSuite.*
import com.evolutiongaming.catshelper.CatsHelper.*

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
      ("failure", Error.raiseError[IO, Unit], failure[Unit](Error)),
      (
        "success-big-stack",

        for {
          ref <- Ref.of[IO, Int](0)
          _ <- Vector.fill(100000)(1).traverse_[IO, Unit](n => ref.update(_ + n))
          result <- ref.get
        } yield result,

        success(100000),
      ),
    )
  } {
    test(name) {
      value.toTry shouldEqual expected
    }
  }


  private case object Error extends RuntimeException with NoStackTrace
}