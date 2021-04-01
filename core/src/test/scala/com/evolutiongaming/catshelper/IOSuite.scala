package com.evolutiongaming.catshelper

import cats.effect.IO
import org.scalatest.Succeeded

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

import cats.effect.unsafe.implicits.global

object IOSuite {
  val Timeout: FiniteDuration = 5.seconds

  implicit val executor: ExecutionContextExecutor = ExecutionContext.global

  def runIO[A](io: IO[A], timeout: FiniteDuration = Timeout): Future[Succeeded.type] = {
    io.timeout(timeout).as(Succeeded).unsafeToFuture()
  }

  implicit class IOOps[A](val self: IO[A]) extends AnyVal {
    def run(timeout: FiniteDuration = Timeout): Future[Succeeded.type] = runIO(self, timeout)
  }
}
