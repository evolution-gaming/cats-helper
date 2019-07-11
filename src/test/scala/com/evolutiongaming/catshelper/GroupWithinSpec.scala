package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, IO, Timer}
import cats.implicits._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.duration._

class GroupWithinSpec extends AsyncFunSuite with Matchers {

  test("support settings = 0") {
    `support settings = 0`[IO].run()
  }

  test("collect until size reached") {
    `collect until size reached`[IO].run()
  }

  test("collect until deadline reached") {
    `collect until deadline reached`[IO].run()
  }

  test("consume on release") {
    `consume on release`[IO].run()
  }

  private def `support settings = 0`[F[_] : Concurrent : Timer] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 0)
    for {
      ref         <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      _           <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
        } yield {}
      }
      a          <- ref.get
    } yield {
      a shouldEqual List(Nel.of(2), Nel.of(1))
    }
  }

  private def `collect until size reached`[F[_] : Concurrent : Timer] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 2)
    for {
      ref         <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      _           <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
          _ <- enqueue(3)
          _ <- enqueue(4)
        } yield {}
      }
      a          <- ref.get
    } yield {
      a shouldEqual List(Nel.of(3, 4), Nel.of(1, 2))
    }
  }

  private def `collect until deadline reached`[F[_] : Concurrent : Timer] = {
    val settings = GroupWithin.Settings(delay = 300.millis, size = 100)
    for {
      deferred    <- Deferred[F, Nel[Int]]
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => deferred.complete(a) }
      a           <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
          a <- deferred.get
        } yield a
      }
    } yield {
      a shouldEqual Nel.of(1, 2)
    }
  }

  private def `consume on release`[F[_] : Concurrent : Timer] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 100)
    for {
      deferred    <- Deferred[F, Nel[Int]]
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => deferred.complete(a) }
      _           <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
        } yield {}
      }
      a          <- deferred.get
    } yield {
      a shouldEqual Nel.of(1, 2)
    }
  }
}
