package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.IORuntime

import scala.concurrent.duration._
import scala.util.{Success, Try}

trait ToTry[F[_]] {

  def apply[A](fa: F[A]): Try[A]
}

object ToTry {

  def apply[F[_]](
    implicit
    F: ToTry[F],
  ): ToTry[F] = F

  def summon[F[_]](
    implicit
    F: ToTry[F],
  ): ToTry[F] = F

  def functionK[F[_]: ToTry]: FunctionK[F, Try] = new FunctionK[F, Try] {

    def apply[A](fa: F[A]): Try[A] = ToTry.summon[F].apply(fa)
  }

  /**
   * Please think twice before using this, ideally you should not have toTry in your `pure` code
   * base!
   *
   * Simple effects (`pure`, `delay`, `map`, `flatMap`, `attempt`, `handleErrorWith`) run on the
   * calling thread and never time out. From the first asynchronous boundary onwards the effect runs
   * as a fiber under
   * [[https://typelevel.org/cats-effect/docs/datatypes/io#scalatimeout IO.timeout]].
   *
   * @param timeout
   *   applies to the fiber part. On expiry the fiber is cancelled, its finalizers complete, and
   *   then `Failure(TimeoutException)` is returned. Inside an
   *   [[https://typelevel.org/cats-effect/docs/typeclasses/monadcancel#uncancelable-regions uncancelable region]]
   *   there is nothing to cancel, so the effect runs to completion regardless of the timeout.
   */
  def ioToTry(
    timeout: FiniteDuration,
  )(implicit
    runtime: IORuntime,
  ): ToTry[IO] = new ToTry[IO] {

    def apply[A](fa: IO[A]): Try[A] = Try {
      fa.syncStep(Int.MaxValue).unsafeRunSync() match {
        case Right(a) => a
        case Left(rest) => rest.timeout(timeout).unsafeRunSync()
      }
    }
  }

  implicit def ioToTry(
    implicit
    ioRuntime: IORuntime,
  ): ToTry[IO] = ioToTry(1.minute)

  implicit val idToTry: ToTry[Id] = new ToTry[Id] {
    def apply[A](fa: Id[A]): Try[A] = Success(fa)
  }

  implicit val tryToTry: ToTry[Try] = new ToTry[Try] {
    def apply[A](fa: Try[A]) = fa
  }

}
