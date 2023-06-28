package com.evolution.catshelper

import cats.{Applicative, Functor}
import cats.effect.syntax.all._
import cats.effect.{Async, Deferred, MonadCancel, Ref, Resource}
import cats.syntax.all._

/**
  * Analog of [[cats.effect.std.CountDownLatch]] that supports increases of latches after creation via method [[CountLatch.acquire]]
  *
  * Example:
  * {{{
  *    import cats.effect.IO
  *    for {
  *       latch <- CountLatch[IO](1)
  *       fiber <- latch.await.start
  *       _ <- latch.acquire()
  *       _ <- latch.release
  *       _ <- latch.release
  *       _ <- fiber.joinWithNever
  *    } yield {}
  * }}}
  *
  * @tparam F effect type, expected to be [[cats.effect.IO]]
  */
sealed trait CountLatch[F[_]] {

  /** Increase latches on [[n]] */
  def acquire(n: Int = 1): F[Unit]

  /** Decrease latches on [[n]] */
  def release(n: Int = 1): F[Unit]

  /** Semantically blocks fiber while latches more than zero */
  def await(): F[Unit]
}

object CountLatch {

  def empty[F[_]: Applicative]: CountLatch[F] =
    new CountLatch[F] {

      val F = Applicative[F]

      override def acquire(n: Int): F[Unit] = F.unit

      override def release(n: Int): F[Unit] = F.unit

      override def await(): F[Unit] = F.unit
    }

  def apply[F[_]: Async](n: Int = 0): F[CountLatch[F]] = {

    sealed trait State
    case object Done extends State
    case class Awaiting(latches: Int, signal: Deferred[F, Unit]) extends State
    object Awaiting {
      def apply(latches: Int): F[State] =
        for {
          await <- Deferred[F, Unit]
        } yield Awaiting(latches, await)
    }

    for {
      state <- if (n > 0) Awaiting(n) else Done.pure[F].widen[State]
      state <- Ref.of[F, State](state)
    } yield
      new CountLatch[F] {

        val F = Async[F]

        override def acquire(n: Int): F[Unit] =
          if (n < 1) F.unit
          else
            state.access
              .flatMap {
                case (state, set) =>
                  for {
                    state <- state match {
                      case Done           => Awaiting(n)
                      case Awaiting(l, a) => Awaiting(l + n, a).pure[F]
                    }
                    result <- set(state)
                  } yield result
              }
              .iterateUntil(identity)
              .void
              .uncancelable

        override def release(n: Int): F[Unit] =
          state
            .modify {
              case Done => Done -> F.unit
              case Awaiting(l, await) =>
                if (l > n) Awaiting(l - n, await) -> F.unit
                else Done -> await.complete(()).void
            }
            .flatten
            .uncancelable

        override def await(): F[Unit] =
          state.get.flatMap {
            case Done                => F.unit
            case Awaiting(_, signal) => signal.get
          }
      }
  }

  implicit final class CountLatchOps[F[_]](val latch: CountLatch[F])
      extends AnyVal {

    /** create [[Resource]] that will acquire & release the latch */
    def asResource(implicit F: Functor[F]): Resource[F, Unit] =
      Resource.make[F, Unit](latch.acquire())(_ => latch.release())

    /** acquire CountLatch before [[fa]] and release it after, tolerating failure & cancellation of [[fa]] */
    def surround[A](fa: F[A])(implicit F: MonadCancel[F, Throwable]): F[A] =
      F.bracket(latch.acquire())(_ => fa)(_ => latch.release())

  }

}
