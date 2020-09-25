package com.evolutiongaming.catshelper

import cats.effect.{IO, Resource}
import cats.implicits._
import com.evolutiongaming.catshelper.CatsHelper._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.TimeoutException
import scala.concurrent.duration._


class ResourceBreakFlatMapChainTest extends AsyncFunSuite with Matchers {
  import ResourceBreakFlatMapChainTest._

  test("stackOverflow") {
    val result = ()
      .pure[IO]
      .toResource
      .multiply(1000)
      .multiply(1000)
      .use { _ => ().pure[IO] }
      .attempt
      .void

    FromFuture[IO]
      .apply { result.toFuture }
      .timeout(3.second)
      .attempt
      .flatMap { a => IO { a should matchPattern { case Right(()) => } } }
      .run()
  }

  test("breakFlatMapChain") {
    ()
      .pure[IO]
      .toResource
      .multiply(100)
      .breakFlatMapChain
      .multiply(100)
      .use { _ => ().pure[IO] }
      .attempt
      .flatMap { a => IO { a shouldEqual ().asRight } }
      .run()
  }
}

object ResourceBreakFlatMapChainTest {

  implicit class ResourceOps(val self: Resource[IO, Unit]) extends AnyVal {

    def multiply(n: Int): Resource[IO, Unit] = {
      List
        .fill(n)(self)
        .foldMapM(identity)
    }
  }
}
