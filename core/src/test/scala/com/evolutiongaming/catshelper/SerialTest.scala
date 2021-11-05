package com.evolutiongaming.catshelper

import cats.effect.kernel.{Deferred, Ref}
import cats.effect.{IO, Sync}
import cats.implicits._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.control.NoStackTrace
import cats.effect.kernel.Async

class SerialTest extends AsyncFunSuite with Matchers {

  test("serial") {
    serial[IO].run()
  }

  test("error") {
    error[IO].run()
  }

  test("asyncBoundary (nice to have)") {
    asyncBoundary[IO].run()
  }


  private def serial[F[_]: Async] = {
    for {
      ref    <- Ref[F].of(List.empty[Int])
      serial <- Serial.of[F]
      d0     <- Deferred[F, Unit]
      _      <- serial { d0.get *> ref.update { 0 :: _ } }
      _      <- serial { ref.update { 1 :: _ } }
      _      <- serial { ref.update { 2 :: _ } }
      f3     <- serial { ref.update { 3 :: _ } }
      _      <- d0.complete(())
      _      <- f3
      a      <- ref.get
      _       = a shouldEqual List(3, 2, 1, 0)
    } yield {}
  }

  private def asyncBoundary[F[_]: Async] = {
    val threadId = Sync[F].delay { Thread.currentThread().getId }
    for {
      serial <- Serial.of[F]
      d      <- Deferred[F, Unit]
      _      <- serial { d.get }
      a      <- serial { threadId }
      b      <- serial { threadId }
      _      <- d.complete(())
      a      <- a
      b      <- b
      _       = a shouldEqual b
    } yield {}
  }

  private def error[F[_]: Async] = {
    val error: Throwable = new RuntimeException with NoStackTrace
    for {
      ref    <- Ref[F].of(List.empty[Int])
      serial <- Serial.of[F]
      d0     <- Deferred[F, F[Unit]]
      f0     <- serial { d0.get.flatten *> ref.update { 0 :: _ } }
      _      <- serial { ref.update { 1 :: _ } }
      _      <- serial { ref.update { 2 :: _ } }
      f3     <- serial { ref.update { 3 :: _ } }
      _      <- d0.complete(error.raiseError[F, Unit])
      a      <- f0.attempt
      _       = a shouldEqual error.asLeft
      _      <- f3
      a      <- ref.get
      _       = a shouldEqual List(3, 2, 1)
    } yield {}
  }
}