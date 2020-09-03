package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers
import com.evolutiongaming.catshelper.IOSuite._

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.control.NoStackTrace


class MemoizeTest extends AsyncFunSuite with Matchers {

  test("concurrent") {
    val result = for {
      counter  <- Ref[IO].of(0)
      deferred <- Deferred[IO, Unit]
      ref      <- Memoize[IO].of { counter.updateAndGet { _ + 1 }.productL(deferred.get) }
      f0       <- ref.start
      f1       <- ref.start
      a        <- f0.join.timeout(10.millis).attempt
      _         = a should matchPattern { case Left(_: TimeoutException) => }
      _        <- deferred.complete(())
      a        <- f0.join
      _         = a shouldEqual 1
      a        <- f1.join
      _         = a shouldEqual 1
      a        <- counter.get
      _         = a shouldEqual 1
    } yield {}
    result.run()
  }

  test("concurrent cache error") {
    val error: Throwable = new RuntimeException with NoStackTrace
    val result = for {
      counter <- Ref[IO].of(0)
      ref     <- Memoize[IO].of { counter.updateAndGet { _ + 1 }.flatMap { _ => error.raiseError[IO, Int]} }
      a       <- ref.attempt
      _        = a shouldEqual error.asLeft
      a       <- ref.attempt
      _        = a shouldEqual error.asLeft
      a       <- counter.get
      _        = a shouldEqual 1
    } yield {}
    result.run()
  }

  test("sync") {
    val result = for {
      counter <- Ref[IO].of(0)
      ref     <- Memoize[IO].of { counter.updateAndGet { _ + 1 } }
      a       <- ref
      _        = a shouldEqual 1
      a       <- ref
      _        = a shouldEqual 1
      a       <- counter.get
      _        = a shouldEqual 1
    } yield {}
    result.run()
  }

  test("sync cache error") {
    val error: Throwable = new RuntimeException with NoStackTrace
    val result = for {
      counter <- Ref[IO].of(0)
      ref     <- Memoize.sync[IO, Int] { counter.updateAndGet { _ + 1 }.flatMap { _ => error.raiseError[IO, Int]} }
      a       <- ref.attempt
      _        = a shouldEqual error.asLeft
      a       <- ref.attempt
      _        = a shouldEqual error.asLeft
      a       <- counter.get
      _        = a shouldEqual 1
    } yield {}
    result.run()
  }
}
