package com.evolutiongaming.catshelper

import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.IORuntime
import cats.implicits._
import com.evolutiongaming.catshelper.testkit.PureTest.ioTest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.concurrent.duration._

class SerialRefSpec extends AnyFreeSpec {
  implicit val ioRuntime: IORuntime = IORuntime.global

  "modify serially" in ioTest { env =>
    import env._

    for {
      ref0     <- SerialRef[IO].of(0)
      ref       = ref0.mapK(FunctionK.id, FunctionK.id) // make sure mapK works
      runCount <- Ref[IO].of(0)

      // As a side-effect we increment run count. SerialRef makes sure it happens exactly once.
      inc       = ref.update(x => runCount.update(_ + 1) *> IO.sleep(1.second) as (x + 1))
      expected  = 1000
      _        <- List.fill(expected)(inc).parSequence_

      _        <- (ref.get, runCount.get).tupled.map(_ shouldBe (expected -> expected))
    } yield ()
  }
}
