package com.evolutiongaming.catshelper

import cats.effect.IO
import org.scalatest.funsuite.AsyncFunSuite
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class CountLatchSpec extends AsyncFunSuite with Matchers {

  import CountLatchSpec.CountLatchOps

  test("init with 0") {
    val io = for {
      latch <- CountLatch[IO](0)
      done <- latch.done
    } yield {
      done shouldBe true
    }
    io.run()
  }

  test("init with 1") {
    val io = for {
      latch <- CountLatch[IO](1)
      blocked <- latch.blocked
      _ <- latch.release()
      done <- latch.done
    } yield {
      blocked shouldBe true
      done shouldBe true
    }
    io.run()
  }

  test("init with 0 and acquire") {
    val io = for {
      latch <- CountLatch[IO](0)
      done0 <- latch.done
      _ <- latch.acquire()
      blocked <- latch.blocked
      _ <- latch.release()
      done1 <- latch.done
    } yield {
      done0 shouldBe true
      blocked shouldBe true
      done1 shouldBe true
    }
    io.run()
  }

  test("init with 1 and acquire") {
    val io = for {
      latch <- CountLatch[IO](1)
      blocked0 <- latch.blocked
      _ <- latch.acquire()
      blocked1 <- latch.blocked
      _ <- latch.release()
      _ <- latch.release()
      done <- latch.done
    } yield {
      blocked0 shouldBe true
      blocked1 shouldBe true
      done shouldBe true
    }
    io.run()
  }

  test("init with 0 and acquire 2") {
    val io = for {
      latch <- CountLatch[IO](0)
      done0 <- latch.done
      _ <- latch.acquire(2)
      blocked <- latch.blocked
      _ <- latch.release()
      _ <- latch.release()
      done1 <- latch.done
    } yield {
      blocked shouldBe true
      done0 shouldBe true
      done1 shouldBe true
    }
    io.run()
  }

  test("ignore acquire -2") {
    val io = for {
      latch <- CountLatch[IO](0)
      done0 <- latch.done
      _ <- latch.acquire(-2)
      done1 <- latch.done
    } yield {
      done0 shouldBe true
      done1 shouldBe true
    }
    io.run()
  }
}

object CountLatchSpec {
  private val immediately = 1.millisecond
  implicit class CountLatchOps(val latch: CountLatch[IO]) extends AnyVal {
    def blocked: IO[Boolean] =
      latch.await().as(false).timeoutTo(immediately, IO(true))
    def done: IO[Boolean] =
      latch.await().as(true).timeoutTo(immediately, IO(false))
  }
}
