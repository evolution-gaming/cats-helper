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
   * Runs the effect on the calling thread for as long as it is pure or `delay`-shaped (`pure`,
   * `delay`, `map`, `flatMap`, `attempt`, `handleErrorWith`, hence also `Ref` operations and
   * `Deferred#complete`). Such effects complete inline, never touch the runtime and never time out.
   * At the first asynchronous boundary, `uncancelable`, `onCancel` or `Resource` allocation, the
   * rest of the effect runs as a fiber on the runtime under `IO.timeout`.
   *
   * @param timeout
   *   bound on the part that runs on the runtime. It is cooperative: where the effect is cancelable
   *   the fiber is cancelled, its finalizers run and the result is `Failure(TimeoutException)`;
   *   inside an `uncancelable` region it waits for the region to end, so a masked effect can run
   *   past it.
   */
  def ioToTry(
    timeout: FiniteDuration,
  )(implicit
    runtime: IORuntime,
  ): ToTry[IO] = new ToTry[IO] {

    def apply[A](fa: IO[A]) = Try {
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
   * `Sync[SyncIO]` that reports a `Cancelable` root scope.
   *
   * `syncStep` steps into `uncancelable` and `onCancel` only when the target's root scope is
   * `Uncancelable`, handing back a remainder without its mask and finalizers. Under this instance
   * it stops at those nodes with the remainder intact.
   *
   * Unlawful for `SyncIO`, hence private and never implicit. The stepper only uses `pure`,
   * `raiseError`, `delay`, `defer`, `realTime`, `monotonic`, `map`, `flatMap`, `handleError` and
   * `handleErrorWith`, so the wrong scope is never observed.
   */
  private object CancelableSyncIO extends Sync[SyncIO] {

    private val F = SyncIO.syncForSyncIO

    def rootCancelScope: CancelScope = CancelScope.Cancelable

    def pure[A](a: A): SyncIO[A] = F.pure(a)

    def raiseError[A](e: Throwable): SyncIO[A] = F.raiseError(e)

    def handleErrorWith[A](fa: SyncIO[A])(f: Throwable => SyncIO[A]): SyncIO[A] = F.handleErrorWith(fa)(f)

    def flatMap[A, B](fa: SyncIO[A])(f: A => SyncIO[B]): SyncIO[B] = F.flatMap(fa)(f)

    def tailRecM[A, B](a: A)(f: A => SyncIO[Either[A, B]]): SyncIO[B] = F.tailRecM(a)(f)

    def suspend[A](hint: Sync.Type)(thunk: => A): SyncIO[A] = F.suspend(hint)(thunk)

    def monotonic: SyncIO[FiniteDuration] = F.monotonic

    def realTime: SyncIO[FiniteDuration] = F.realTime

    def forceR[A, B](fa: SyncIO[A])(fb: SyncIO[B]): SyncIO[B] = F.forceR(fa)(fb)

    def uncancelable[A](body: Poll[SyncIO] => SyncIO[A]): SyncIO[A] = F.uncancelable(body)

    def canceled: SyncIO[Unit] = F.canceled

    def onCancel[A](fa: SyncIO[A], fin: SyncIO[Unit]): SyncIO[A] = F.onCancel(fa, fin)
  }
}
