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
   * A pure or `delay`-shaped effect (`pure`, `delay`, `map`, `flatMap`, `attempt`,
   * `handleErrorWith`, hence also `Ref` operations and `Deferred#complete`) runs on the calling
   * thread: it never reaches the runtime and never times out. From the first asynchronous boundary,
   * `uncancelable`, `onCancel` or `Resource` allocation onwards, the effect runs as a fiber under
   * `IO.timeout`.
   *
   * @param timeout
   *   bound on the part that runs as a fiber. `IO.timeout` cancels rather than abandons: where the
   *   effect is cancelable it stops, its finalizers run, and the result is
   *   `Failure(TimeoutException)`. Cancellation waits for an `uncancelable` region to end, so an
   *   effect can run past the timeout inside one.
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
   * `syncStep` runs an `IO` on the calling thread as far as it can and hands back the rest as an
   * `IO`. When the `Sync` it steps into reports an `Uncancelable` root scope, as `SyncIO`'s own
   * instance does, `syncStep` steps inside `uncancelable` regions (masked regions, in cats-effect
   * terms) and past `onCancel` finalizers, since neither can matter to a target that is never
   * cancelled. What it hands back is then the inside of such a region, without the `uncancelable`
   * around it and without the finalizers. Cancelling that, which is what a timeout does, skips the
   * finalizers.
   *
   * Under this instance `syncStep` stops in front of `uncancelable` and `onCancel` instead, and
   * hands back the region whole.
   *
   * `rootCancelScope` is the only member `syncStep` reads to make that choice. Every other member
   * is here to satisfy `Sync` and delegates to `SyncIO`'s own instance. The instance is unlawful,
   * because `SyncIO#canceled` does nothing and an instance calling `SyncIO` cancelable therefore
   * breaks the `MonadCancel` laws, so it stays private and is never implicit.
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
