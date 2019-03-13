package com.evolutiongaming.catshelper

import cats.effect.{IO, Sync}
import cats.implicits._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.Future
import scala.util.control.NoStackTrace

class FromFutureSpec extends AsyncFunSuite with Matchers {

  for {
    (name, future, expected) <- List(
      ("successful future",       () => Future.successful(()), ().asRight[Throwable]),
      ("failed future",           () => Future.failed(Error),   Error.asLeft[Unit]),
      ("failed to return future", () => throw Error,            Error.asLeft[Unit]),
    )
  } yield {
    test(name) {
      testF[IO](future, expected).run()
    }
  }

  private def testF[F[_] : Sync : FromFuture](
    future: () => Future[Unit],
    expected: Either[Throwable, Unit]
  ) = {
    val fa = FromFuture[F].apply(future())
    for {
      actual <- fa.attempt
    } yield {
      actual shouldEqual expected
    }
  }

  private case object Error extends RuntimeException with NoStackTrace
}
