package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalatest.Succeeded

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

object IOSuite {
  val Timeout: FiniteDuration = 5.seconds

  implicit val ioRuntime: IORuntime = IORuntime.global
  implicit val executor: ExecutionContextExecutor = ExecutionContext.global

  def runIO[A](io: IO[A], timeout: FiniteDuration = Timeout): Future[Succeeded.type] = {
    io.timeout(timeout).as(Succeeded).unsafeToFuture()
  }

  implicit class IOOps[A](val self: IO[A]) extends AnyVal {
    def run(timeout: FiniteDuration = Timeout): Future[Succeeded.type] = runIO(self, timeout)

    def eventually(attemptInterval: FiniteDuration = 10.millis): IO[A] = {
      def attempt: IO[A] = self.attempt.flatMap {
        case Left(_) => IO.sleep(attemptInterval) >> attempt
        case Right(a) => IO.pure(a)
      }

      attempt
    }

  }
}
