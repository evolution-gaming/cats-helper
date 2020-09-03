package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.syntax.all._
import com.evolutiongaming.catshelper.CatsHelper._

import scala.util.control.NoStackTrace
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

class ToFutureSpec extends AsyncFunSuite with Matchers {

  for {
    (name, value, expected) <- List(
      ("success", ().pure[IO], ().asRight[Throwable]),
      ("failure", Error.raiseError[IO, Unit], Error.asLeft[Unit])
    )
  } {
    test(name) {
      val either = for {
        value <- value
      } yield {
        value.asRight[Throwable]
      }

      val future = either.toFuture

      future.value.isDefined shouldEqual true

      for {
        actual <- future.recover { case error => error.asLeft }
      } yield {
        actual shouldEqual expected
      }
    }
  }

  test("functionK") {
    val functionK = ToFuture.summon[IO].toFunctionK
    for {
      a <- functionK(0.pure[IO])
    } yield {
      a shouldEqual 0
    }
  }

  private case object Error extends RuntimeException with NoStackTrace
}