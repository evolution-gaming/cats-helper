package com.evolutiongaming.catshelper

import cats.effect.{Async, Deferred, Ref}
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

  /** Decrease latches by one */
  def release(): F[Unit]

  /** Semantically blocks fiber while latches more than zero */
  def await(): F[Unit]
}

object CountLatch {

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
            F.uncancelable { _ =>
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
            }

        override def release(): F[Unit] =
          F.uncancelable { _ =>
            state.modify {
              case Done               => Done -> F.unit
              case Awaiting(1, await) => Done -> await.complete(()).void
              case Awaiting(n, await) => Awaiting(n - 1, await) -> F.unit
            }.flatten
          }

        override def await(): F[Unit] =
          state.get.flatMap {
            case Done                => F.unit
            case Awaiting(_, signal) => signal.get
          }
      }
  }

}
