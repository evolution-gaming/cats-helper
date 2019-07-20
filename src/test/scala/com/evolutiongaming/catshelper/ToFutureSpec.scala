package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.implicits._
import com.evolutiongaming.catshelper.EffectHelper._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.util.control.NoStackTrace

class ToFutureSpec extends AsyncFunSuite with Matchers {

  for {
    (name, value, expected) <- List(
      ("success", ().pure[IO],                ().asRight[Throwable]),
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


  private case object Error extends RuntimeException with NoStackTrace
}