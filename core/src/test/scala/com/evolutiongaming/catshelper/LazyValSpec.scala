package com.evolutiongaming.catshelper

import cats.arrow.FunctionK
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.{IO, Sync}
import cats.implicits._
import com.evolutiongaming.catshelper.CatsHelper._
import com.evolutiongaming.catshelper.IOSuite._

import scala.util.control.NoStackTrace
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers
import cats.effect.kernel.Async

class LazyValSpec extends AsyncFunSuite with Matchers {

  test("get") {
    get[IO].run()
  }

  test("get error") {
    getError[IO].run()
  }

  test("getLoaded") {
    getLoaded[IO].run()
  }

  private def get[F[_] : Async]: F[Unit] = {
    for {
      ref      <- Ref[F].of(0)
      deferred <- Deferred[F, Unit]
      load      = for {
        _ <- ref.update(_ + 1)
        a <- deferred.get
      } yield a

      lazyRef0 <- LazyVal.of(load)
      lazyRef   = lazyRef0.mapK(FunctionK.id)
      count    <- ref.get
      _        <- Sync[F].delay { count shouldEqual 0 }
      a0       <- lazyRef.get.startEnsure
      a1       <- lazyRef.get.startEnsure
      _        <- deferred.complete(())
      _        <- a0.join
      _        <- a1.join
      count    <- ref.get
      _        <- Sync[F].delay { count shouldEqual 1 }
    } yield {}
  }

  private def getError[F[_] : Async]: F[Unit] = {

    case object Error extends RuntimeException with NoStackTrace

    for {
      ref     <- Ref[F].of(0)
      load     = for {
        _ <- ref.update(_ + 1)
        a <- Error.raiseError[F, Unit]
      } yield a
      lazyRef <- LazyVal.of(load)
      count   <- ref.get
      _       <- Sync[F].delay { count shouldEqual 0 }
      a       <- lazyRef.get.attempt
      _       <- Sync[F].delay { a shouldEqual Error.asLeft[Unit] }
      a       <- lazyRef.get.attempt
      _       <- Sync[F].delay { a shouldEqual Error.asLeft[Unit] }
      count   <- ref.get
      _       <- Sync[F].delay { count shouldEqual 1 }
    } yield {}
  }

  private def getLoaded[F[_] : Async] = {
    for {
      ref     <- Ref[F].of(0)
      load     = ref.update(_ + 1)
      lazyRef <- LazyVal.of(load)
      count   <- ref.get
      _       <- Sync[F].delay { count shouldEqual 0 }
      a       <- lazyRef.getLoaded
      _       <- Sync[F].delay { a shouldEqual none[Unit] }
      count   <- ref.get
      _       <- Sync[F].delay { count shouldEqual 0 }
      _       <- lazyRef.get
      a       <- lazyRef.getLoaded
      _       <- Sync[F].delay { a shouldEqual ().some }
    } yield {}
  }
}