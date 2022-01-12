package com.evolutiongaming.catshelper.testkit

import cats.effect.IO
import cats.effect.kernel.Ref
import com.evolutiongaming.catshelper.testkit.PureTest.ioTest
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
       PureTest.hotLoopTimeout(100.millis).ioTest { _ => IO.unit.foreverM }
     }
   }

   "flakiness check" in {
     // Usually flakiness comes from undefined concurrent execution order.
     // Here we simulate it with a shared mutable state.
     val counter = Ref.unsafe[IO, Int](0)
     assertThrows[TestFailedException] {
       PureTest.flakinessCheckIterations(5).ioTest { _ =>
         counter.modify(i => (i + 1) -> i).map(_ should not be (4))
       }
     }
   }

  "is unaffected by infinite background loops" in ioTest { _ =>
    val main = IO.sleep(1.milli)
    val loop = IO.sleep(1.nano).foreverM
    loop.start *> main
  }
}
