package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.implicits._
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.std.Semaphore
import cats.effect.{Concurrent, Resource, Temporal}
import cats.implicits._
import cats.{Applicative, ~>}

import scala.concurrent.duration._

/**
 * Collects elements into batches and passes each batch to a handler.
 *
 * A batch is closed when it reaches [[GroupWithin.Settings.size]] elements, or when
 * [[GroupWithin.Settings.delay]] passes after the first element of the batch, whichever happens
 * first.
 *
 * The guarantees around serialisation, cancellation and release depend on the implementation. Only
 * the batching one gives them, see [[GroupWithin.apply]] and [[GroupWithin.empty]].
 *
 * {{{
 * GroupWithin[IO]
 *   .apply[Int](GroupWithin.Settings(delay = 100.millis, size = 10)) { batch => store(batch) }
 *   .use { enqueue => enqueue(1) *> enqueue(2) }
 * }}}
 */
trait GroupWithin[F[_]] {
  import GroupWithin._

  /**
   * @param settings
   *   batch size and delay
   * @param f
   *   handler for a closed batch, called serially
   * @return
   *   an [[GroupWithin.Enqueue]], releasing the resource flushes the pending batch
   */
  def apply[A](settings: Settings)(f: Nel[A] => F[Unit]): Resource[F, Enqueue[F, A]]
}

object GroupWithin {

  /**
   * A batch is closed by whichever limit is reached first, it does not wait for both.
   *
   * @param delay
   *   time to wait after the first element of a batch before the batch is closed
   * @param size
   *   number of elements that closes a batch
   */
  final case class Settings(delay: FiniteDuration, size: Int)

  /**
   * Does not batch, ignores the settings and calls the handler with one element at a time.
   *
   * The handler runs on the enqueueing fiber, so calls are not serialised. The resource holds no
   * state, so release does nothing and enqueue keeps working after it.
   */
  def empty[F[_]]: GroupWithin[F] = new GroupWithin[F] {

    def apply[A](settings: Settings)(f: Nel[A] => F[Unit]): Resource[F, Enqueue[F, A]] = {
      val enqueue = new Enqueue[F, A] {
        def apply(a: A) = f(Nel.of(a))
      }
      Resource.pure(enqueue)
    }
  }

  /**
   * With `size <= 1` or `delay <= 0` there is nothing to batch, so the handler is called directly
   * per element and no state is allocated. That path behaves like [[GroupWithin.empty]] and gives
   * none of the guarantees below.
   *
   * While batching:
   *   - handler calls are serialised, so batches do not overlap, but the order of batches is not
   *     guaranteed;
   *   - enqueue is uncancelable, so an accepted element is always part of a batch. The handler runs
   *     inside that region, so an enqueue that closes a batch cannot be cancelled while the handler
   *     is blocked;
   *   - after release, enqueue silently discards the element;
   *   - release closes a pending batch and waits for every batch to be handed to the handler and
   *     returned, including a batch the delay timer already took. The wait has no bound and runs in
   *     a resource finalizer, so a handler that never returns blocks release. An error the handler
   *     raises on the timer path is not reported to release.
   */
  def apply[F[_]: Temporal]: GroupWithin[F] = {

    new GroupWithin[F] {

      def apply[A](settings: Settings)(f: Nel[A] => F[Unit]) = {

        val void = ().pure[F]

        sealed trait S

        object S {
          def empty: S = Empty
          def stopped: S = Stopped
          def full(a: A, closed: Deferred[F, Unit]): S = Full(Nel.of(a), 1, closed)

          case object Empty extends S
          case object Stopped extends S

          /**
           * @param size
           *   number of elements in `as`, kept in step by `append` so that enqueue does not
           *   traverse the batch
           * @param closed
           *   completed when the batch is closed by size or by release, which stops its timer. Also
           *   identifies the batch, so a timer only closes the batch it was started for
           */
          final case class Full(
            as: Nel[A],
            size: Int,
            closed: Deferred[F, Unit],
          ) extends S {
            def append(a: A): Full = Full(a :: as, size + 1, closed)
            def isFilled: Boolean = size >= settings.size
          }
        }

        if (settings.size <= 1 || settings.delay <= 0.millis) {
          val enqueue: Enqueue[F, A] = a => f(Nel.of(a))
          Resource.pure(enqueue)
        } else {
          val result = for {
            semaphore <- Semaphore[F](1)
            drained <- Deferred[F, Unit]
            ref <- Ref[F].of((S.empty, 0))
          } yield {

            def consume(as: Nel[A]) = semaphore.permit.use { _ => f(as.reverse) }
              .guarantee {
                ref.modify { case (s, inFlight) =>
                  val remaining = inFlight - 1
                  val signal = s match {
                    case S.Stopped if remaining == 0 => drained.complete(()).void
                    case _ => void
                  }
                  ((s, remaining), signal)
                }
                  .flatten
              }

            def startTimer(closed: Deferred[F, Unit]) = {
              val result = for {
                _ <- Temporal[F].race(Temporal[F].sleep(settings.delay), closed.get)
                a <- ref.modify {
                  case (s: S.Full, inFlight) if s.closed.eq(closed) =>
                    ((S.empty, inFlight + 1), consume(s.as))
                  case s => (s, void)
                }
                a <- a
              } yield a
              result
                .start
                .void
            }

            // An open batch already has its own `closed`, so the allocation happens only on the
            // path that opens one. The state is read again after the allocation, because another
            // fiber can open the batch in between.
            def openBatch(element: A): F[Unit] = {
              for {
                closed <- Deferred[F, Unit]
                _ <- ref.modify {
                  case (S.Empty, inFlight) => ((S.full(element, closed), inFlight), startTimer(closed))
                  case (s: S.Full, inFlight) =>
                    val full = s.append(element)
                    if (full.isFilled) ((S.empty, inFlight + 1), s.closed.complete(()) *> consume(full.as))
                    else ((full, inFlight), void)
                  case (S.Stopped, inFlight) => ((S.stopped, inFlight), void)
                }.flatten
              } yield ()
            }

            val enqueue = new Enqueue[F, A] {

              def apply(a: A) = {
                Concurrent[F].uncancelable { _ =>
                  ref.modify {
                    case (s: S.Full, inFlight) =>
                      val full = s.append(a)
                      if (full.isFilled) ((S.empty, inFlight + 1), s.closed.complete(()) *> consume(full.as))
                      else ((full, inFlight), void)
                    case (S.Empty, inFlight) => ((S.empty, inFlight), openBatch(a))
                    case (S.Stopped, inFlight) => ((S.stopped, inFlight), void)
                  }
                    .flatten
                }
              }
            }

            val release = ref.modify {
              case (s: S.Full, inFlight) => ((S.stopped, inFlight + 1), s.closed.complete(()) *> consume(s.as))
              case (_, inFlight) => ((S.stopped, inFlight), void)
            }
              .flatten
              .guarantee { ref.get.flatMap { case (_, inFlight) => drained.get.whenA(inFlight > 0) } }

            (enqueue, release)
          }

          Resource(result)
        }
      }
    }
  }

  trait Enqueue[F[_], A] {

    def apply(a: A): F[Unit]
  }

  object Enqueue {

    def empty[F[_]: Applicative, A]: Enqueue[F, A] = const[F, A](().pure[F])

    def const[F[_], A](value: F[Unit]): Enqueue[F, A] = _ => value

    implicit class EnqueueOps[F[_], A](val self: Enqueue[F, A]) extends AnyVal {

      def mapK[G[_]](f: F ~> G): Enqueue[G, A] = (a: A) => f(self(a))
    }
  }
}
