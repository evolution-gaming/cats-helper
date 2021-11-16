package com.evolutiongaming.catshelper

import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LogOfSafeSpec extends AnyFunSuite with Matchers {
  class SomeService[F[_] : LogOf.Safe] {
    val log: Log[F] = implicitly[LogOf.Safe[F]].apply[SomeService[F]]

    def logIt(arg: String): F[Unit] = log.info("this it message: " + arg)
  }

  test("POC") {
    implicit val logOfSafe: LogOf.Safe[IO] = LogOf.slf4jSafe[IO].unsafeRunSync()

    new SomeService[IO].logIt("Wow such simple no F[Log[F]]")

  }

}
