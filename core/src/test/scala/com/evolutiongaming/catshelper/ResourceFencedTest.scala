package com.evolutiongaming.catshelper

import cats.effect.Resource
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.IO
import cats.implicits._
import com.evolutiongaming.catshelper.IOSuite._
import com.evolutiongaming.catshelper.CatsHelper._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers


class ResourceFencedTest extends AsyncFunSuite with Matchers {

  test("release happens once") {
    val result = for {
      counter      <- Ref[IO].of(0)
      resource      = Resource.make(().pure[IO]) { _ => counter.update(_ + 1) }
      ab           <- resource.fenced.allocated
      (_, release)  = ab
      _            <- release
      _            <- release
      count        <- counter.get
      _             = count shouldEqual 1
    } yield {}
    result.run()
  }

  test("duplicate release call still waits until released") {
    val result = for {
      counter   <- Ref[IO].of(0)
      deferred  <- Deferred[IO, Unit]
      resource   = Resource.make(().pure[IO]) { _ => deferred.get *> counter.update(_ + 1) }
      ab        <- resource.fenced.allocated
      (_, release)  = ab
      release1 = {
        for {
          released <- Ref[IO].of(false)
          release1  = for {
            _ <- release
            _ <- released.set(true)
          } yield {}
          fiber    <- release1.startEnsure
        } yield {
          (released.get, fiber)
        }
      }
      ab1       <- release1
      (released1, fiber1) = ab1

      ab2       <- release1
      (released2, fiber2) = ab2

      released  <- released1
      _         = released shouldEqual false

      released <- released2
      _         = released shouldEqual false

      _        <- deferred.complete(())

      _        <- fiber1.join
      released <- released1
      _         = released shouldEqual true

      _        <- fiber2.join
      released <- released1
      _         = released shouldEqual true

      count     <- counter.get
      _          = count shouldEqual 1
    } yield {}
    result.run()
  }
}
