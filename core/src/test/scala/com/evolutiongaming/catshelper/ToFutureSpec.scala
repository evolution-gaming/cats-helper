package com.evolutiongaming.catshelper

import cats.effect.{IO, Ref}
import cats.effect.unsafe.IORuntime
import cats.implicits.*
import com.evolutiongaming.catshelper.CatsHelper.*
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.control.NoStackTrace

class ToFutureSpec extends AsyncFunSuite with Matchers {
  implicit val ioRuntime: IORuntime = IORuntime.global

  for {
    (name, value, expected) <- List(
      ("success", ().pure[IO], ().asRight[Throwable]),
      ("failure", Error.raiseError[IO, Unit], Error.asLeft[Unit]),
      (
        "success-big-stack",

        for {
          ref <- Ref.of[IO, Int](0)
          _ <- Vector.fill(100000)(1).traverse_[IO, Unit](n => ref.update(_ + n))
          result <- ref.get
        } yield result,

        100000.asRight[Throwable]
      ),
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