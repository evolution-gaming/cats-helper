package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LogbackSpec extends AnyFunSuite with Matchers {

  test("logback implementation") {
    val io = for {
      logOf <- Logback[IO]
      log <- logOf(getClass)
      _ <- log.info("hello from logback", Log.Mdc("k" -> "test value for K"))
    } yield ()

    io.unsafeRunSync()
  }

}
