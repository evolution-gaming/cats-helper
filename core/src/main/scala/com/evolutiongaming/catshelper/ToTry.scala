package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import cats.effect.kernel.{CancelScope, Poll, Sync}
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, SyncIO}

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
   * A pure or `delay`-shaped effect runs on the calling thread and never times out. That covers
   * `pure`, `delay`, `map`, `flatMap`, `attempt` and `handleErrorWith`, hence `Ref` operations and
   * `Deferred#complete` too. Everything from the first asynchronous boundary, `uncancelable` or
   * `onCancel` runs as a fiber under `IO.timeout`. A `Resource` brings the last two with it.
   *
   * @param timeout
   *   applies to the part that runs as a fiber. The effect is cancelled, not abandoned: it stops at
   *   its next cancelable point, its finalizers run, and the result is `Failure(TimeoutException)`.
   *   An `uncancelable` region has no such point, so an effect can outlive the timeout inside one.
   */
  def ioToTry(
    timeout: FiniteDuration,
  )(implicit
    runtime: IORuntime,
  ): ToTry[IO] = new ToTry[IO] {

    def apply[A](fa: IO[A]): Try[A] = Try {
      IO.asyncForIO.syncStep[SyncIO, A](fa, Int.MaxValue)(CancelableSyncIO).unsafeRunSync() match {
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

  /**
   * `Sync[SyncIO]` that reports a `Cancelable` root scope, for `IO.syncStep` and nothing else.
   *
   * `syncStep` runs an `IO` on the calling thread as far as it can and returns the rest as an `IO`.
   * With an `Uncancelable` root scope, which `SyncIO`'s own instance reports, it steps inside
   * `uncancelable` regions and past `onCancel` finalizers. The `IO` it returns has lost both, and a
   * timeout that cancels it skips the finalizers. Under this instance `syncStep` stops in front of
   * `uncancelable` and `onCancel` and returns the region whole.
   *
   * `rootCancelScope` is the only member `syncStep` reads for that; the others delegate to
   * `SyncIO`'s own instance. Reporting `SyncIO` as cancelable is false, since `SyncIO#canceled`
   * does nothing, so the instance is unlawful, private and never implicit.
   *
   * @see
   *   [[https://typelevel.org/cats-effect/docs/typeclasses/monadcancel MonadCancel]] for masked
   *   regions, `uncancelable` and finalizers
   */
  private object CancelableSyncIO extends Sync[SyncIO] {

    private val F = SyncIO.syncForSyncIO

    def rootCancelScope: CancelScope =
      CancelScope.Cancelable

    def pure[A](a: A): SyncIO[A] =
      F.pure(a)

    def raiseError[A](e: Throwable): SyncIO[A] =
      F.raiseError(e)

    def handleErrorWith[A](fa: SyncIO[A])(f: Throwable => SyncIO[A]): SyncIO[A] =
      F.handleErrorWith(fa)(f)

    def flatMap[A, B](fa: SyncIO[A])(f: A => SyncIO[B]): SyncIO[B] =
      F.flatMap(fa)(f)

    def tailRecM[A, B](a: A)(f: A => SyncIO[Either[A, B]]): SyncIO[B] =
      F.tailRecM(a)(f)

    def suspend[A](hint: Sync.Type)(thunk: => A): SyncIO[A] =
      F.suspend(hint)(thunk)

    def monotonic: SyncIO[FiniteDuration] =
      F.monotonic

    def realTime: SyncIO[FiniteDuration] =
      F.realTime

    def forceR[A, B](fa: SyncIO[A])(fb: SyncIO[B]): SyncIO[B] =
      F.forceR(fa)(fb)

    def uncancelable[A](body: Poll[SyncIO] => SyncIO[A]): SyncIO[A] =
      F.uncancelable(body)

    def canceled: SyncIO[Unit] =
      F.canceled

    def onCancel[A](fa: SyncIO[A], fin: SyncIO[Unit]): SyncIO[A] =
      F.onCancel(fa, fin)
  }
}
