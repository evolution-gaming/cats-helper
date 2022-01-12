package com.evolutiongaming.catshelper

import cats.effect.kernel.Deferred
import cats.effect.{IO, Resource}
import cats.implicits._
import cats.effect.implicits._
import com.evolutiongaming.catshelper.CatsHelper._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._


class ResourceBreakFlatMapChainTest extends AsyncFunSuite with Matchers {
  import ResourceBreakFlatMapChainTest._

  test("stackOverflow") {
    val result = ()
      .pure[IO]
      .toResource
      .multiply(100)
      .multiply(100)
      .use { _ => ().pure[IO] }
      .attempt
      .void

    FromFuture[IO]
      .apply { result.toFuture }
      .timeout(3.seconds)
      .attempt
      .flatMap { a => IO { a should matchPattern { case Right(()) => } } }
      .run()
  }

  test("cancellable") {
    val result = for {
      started  <- Deferred[IO, Unit]
      released <- Deferred[IO, Unit]
      resource  = for {
        _ <- Resource.release { released.complete(()).void }
        _ <- started.complete(()).toResource
        _ <- IO.never.as(()).toResource
      } yield {}
      fiber    <- resource.breakFlatMapChain.use { _ => ().pure[IO] }.start
      _        <- started.get
      _        <- fiber.cancel
      _        <- released.get
    } yield {}
    result.run()
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
