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
   * Simple effects (`pure`, `delay`, `map`, `flatMap`, `attempt`, `handleErrorWith`, and therefore
   * `Ref` operations and `Deferred#complete`) run on the calling thread and never time out. From
   * the first `uncancelable`, `onCancel`, `Resource` or asynchronous boundary onwards the effect
   * runs as a fiber under
   * [[https://typelevel.org/cats-effect/docs/datatypes/io#scalatimeout IO.timeout]].
   *
   * @param timeout
   *   applies to the part that runs as a fiber. On expiry the fiber is cancelled (not abandoned),
   *   its finalizers run, and the result is `Failure(TimeoutException)`. Inside an
   *   [[https://typelevel.org/cats-effect/docs/typeclasses/monadcancel#uncancelable-regions uncancelable region]]
   *   there is nothing to cancel, so the effect runs to completion regardless of the timeout.
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
   * A `Sync[SyncIO]` whose only purpose is to control where
   * [[https://typelevel.org/cats-effect/api/3.x/cats/effect/kernel/Async.html#syncStep syncStep]]
   * stops walking.
   *
   * `syncStep` walks an `IO` node by node on the calling thread and returns whatever it could not
   * walk as a new `IO`. How far it walks depends on `rootCancelScope` of the `Sync` instance it is
   * given. With `SyncIO`'s own instance (scope = `Uncancelable`) it walks inside `uncancelable`
   * regions and past `onCancel` finalizers, because `SyncIO` can never be cancelled anyway. The
   * `IO` it returns then lacks those protections, and a timeout cancelling it skips the finalizers.
   *
   * This instance reports scope = `Cancelable`, so `syncStep` stops at `uncancelable` and
   * `onCancel` and returns the region as is, protections included.
   *
   * `rootCancelScope` is the only member `syncStep` consults for this decision. Every other member
   * delegates to `SyncIO`'s own `Sync`. The instance is unlawful (`SyncIO` cannot actually be
   * cancelled, which the `Cancelable` scope falsely promises) and therefore private and never
   * implicit.
   *
   * @see
   *   [[https://typelevel.org/cats-effect/docs/typeclasses/monadcancel MonadCancel]] for
   *   cancellation, uncancelable regions and finalizers
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
