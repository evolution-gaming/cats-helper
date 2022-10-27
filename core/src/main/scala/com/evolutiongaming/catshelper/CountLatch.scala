package com.evolutiongaming.catshelper

import cats.effect.{Async, Concurrent, Deferred, Ref}
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
  *       _ <- latch.acquire
  *       _ <- latch.release
  *       _ <- latch.release
  *       _ <- fiber.joinWithNever
  *    } yield {}
  * }}}
  *
  * @tparam F effect type, expected to be [[cats.effect.IO]]
  */
sealed trait CountLatch[F[_]] {

  /** Increase latches by one */
  def acquire: F[Unit]

  /** Decrease latches by one */
  def release: F[Unit]

  /** Semantically blocks fiber while latches more than zero */
  def await: F[Unit]
}

object CountLatch {

  def apply[F[_]: Async](n: Int = 0): F[CountLatch[F]] =
    for {
      state <- if (n > 0) State.awaiting(n) else State.Done[F]().pure[F]
      state <- Ref.of[F, State[F]](state)
    } yield
      new CountLatch[F] {

        override def acquire: F[Unit] =
          state.update {
            case State.Done()             => State.Awaiting(1, Deferred.unsafe[F, Unit])
            case State.Awaiting(n, await) => State.Awaiting(n + 1, await)
          }

        override def release: F[Unit] =
          Async[F].uncancelable { _ =>
            state.modify {
              case d: State.Done[F] => d -> Async[F].unit
              case State.Awaiting(1, await) =>
                State.Done[F]() -> await.complete(()).void
              case State.Awaiting(n, await) =>
                State.Awaiting(n - 1, await) -> Async[F].unit
            }.flatten
          }

        override def await: F[Unit] =
          state.get.flatMap {
            case State.Done()              => Async[F].unit
            case State.Awaiting(_, signal) => signal.get
          }
      }

  private sealed trait State[F[_]]

  private object State {
    final case class Awaiting[F[_]](latches: Int, signal: Deferred[F, Unit])
        extends State[F]

    final case class Done[F[_]]() extends State[F]

    def awaiting[F[_]: Concurrent](latches: Int): F[Awaiting[F]] =
      for {
        await <- Deferred[F, Unit]
      } yield Awaiting(latches, await)
  }

}
