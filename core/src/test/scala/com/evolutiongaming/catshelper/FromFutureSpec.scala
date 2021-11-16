package com.evolutiongaming.catshelper

import cats.effect.{IO, Sync}
import cats.implicits._
import com.evolutiongaming.catshelper.IOSuite._

import scala.concurrent.Future
import scala.util.control.NoStackTrace
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

class FromFutureSpec extends AsyncFunSuite with Matchers {

  for {
    (name, future, expected) <- List(
      ("successful completed", () => Future.successful(()),  ().asRight[Throwable]),
      ("successful running",   () => Future { () },          ().asRight[Throwable]),
      ("failed completed",     () => Future.failed(Error),   Error.asLeft[Unit]),
      ("failed running",       () => Future { throw Error }, Error.asLeft[Unit]),
      ("failed to return",     () => throw Error,            Error.asLeft[Unit]),
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
    val fa = FromFuture.defer[F](future())
    for {
      actual <- fa.attempt
    } yield {
      actual shouldEqual expected
    }
  }

  test("functionK") {
    val functionK = FromFuture.summon[IO].toFunctionK
    val fa = for {
      a <- functionK(Future.successful(0))
    } yield {
      a shouldEqual 0
    }
    fa.run()
  }

  private case object Error extends RuntimeException with NoStackTrace
}
