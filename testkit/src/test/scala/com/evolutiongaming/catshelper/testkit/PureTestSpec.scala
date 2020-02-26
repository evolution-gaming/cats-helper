package com.evolutiongaming.catshelper.testkit

import cats.effect.IO
import cats.implicits._
import com.evolutiongaming.catshelper.testkit.PureTest.{AbnormalTermination, ioTest}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.concurrent.duration._

class PureTestSpec extends AnyFreeSpec {
  "time flows as expected" in ioTest { env =>
    import env._
    for {
      _ <- IO.race(IO.sleep(1.hour), IO.sleep(1.day))
      _ <- testRuntime.getTimeSinceStart.map(_ shouldBe 1.hour)
    } yield ()
  }

  "false assertion fails a test" in {
    assertThrows[TestFailedException] {
      ioTest { _ => IO { 1 shouldBe 2 } }
    }
  }

  "non-termination fails a test" in {
    assertThrows[AbnormalTermination] {
      ioTest { _ => IO.never }
    }
  }

  "hot loop fails a test" in {
    assertThrows[AbnormalTermination] {
      ioTest(hotLoopTimeout = 100.millis) { _ => IO.unit.foreverM }
    }
  }

  "is unaffected by infinite background loops" in ioTest { env =>
    import env._
    val main = IO.sleep(1.milli)
    val loop = IO.sleep(1.nano).foreverM
    loop.start *> main
  }
}
