package com.evolutiongaming.catshelper

import cats.effect.implicits.effectResourceOps
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all._
import com.evolutiongaming.catshelper.IOSuite._
import com.evolutiongaming.catshelper.ResourceRetry.implicits._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

// format: off
class ResourceRetrySpec extends AsyncFunSuite with Matchers {

  val error  = new Exception(s"text exception")
  val config = ResourceRetry.Config("test", 5, 100.millis, 10.millis)

  implicit val log   = Log.empty[IO]
  implicit val retry = ResourceRetry.of[IO](config)

  test("resource should retry on failure in `use`") {
    Ref
      .unsafe[IO, Int](0)
      .updateAndGet(_ + 1)
      .toResource
      .useRetry {
        case i if i < 5 => error.raiseError[IO, Int]
        case result     => result.pure[IO]
      }
      .map { _ shouldBe 5 }
      .run()
  }

  test(s"resource should not retry more than ${config.attempts} times") {
    Ref
      .unsafe[IO, Int](0)
      .updateAndGet(_ + 1)
      .toResource
      .useRetry {
        case i if i < 7 => error.raiseError[IO, Int]
        case result     => result.pure[IO]
      }
      .map { success =>
        fail(s"Successfully finished with value `$success` instead of failing with `$error` as expected")
      }
      .run()
      .recover {
        case `error`                     => succeed
        case failed: TestFailedException => throw failed
        case unexpected                  => fail(s"Do not fail with `$error` as expected", unexpected)
      }
  }

  test("resource should retry even if release fails") {
    val allocate = Ref.unsafe[IO, Int](0).updateAndGet(_ + 1)
    val release  = error.raiseError[IO, Unit]
    Resource
      .make(allocate)(_ => release)
      .useRetry {
        case i if i < 5 => error.raiseError[IO, Int]
        case result     => result.pure[IO]
      }
      .map { _ shouldBe 5 }
      .run()
  }
}
