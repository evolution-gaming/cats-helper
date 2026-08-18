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
 * What is guaranteed around serialisation, cancellation and release depends on the implementation.
 * Only the batching one gives any of it, see `GroupWithin.apply` and `GroupWithin.empty`.
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

        sealed trait Batch

        object Batch {
          def empty: Batch = Empty
          def stopped: Batch = Stopped
          def full(a: A, closed: Deferred[F, Unit]): Batch = Full(Nel.of(a), 1, closed)

          case object Empty extends Batch
          case object Stopped extends Batch

          /**
           * @param size
           *   number of elements in `as`, counted so that enqueue does not traverse the batch
           * @param closed
           *   completed when the batch is closed by size or by release, which stops its timer, and
           *   used as the identity of the batch so that a timer only closes the batch it was
           *   started for
           */
          final case class Full(as: Nel[A], size: Int, closed: Deferred[F, Unit]) extends Batch
        }

        // `inFlight` is the number of batches handed to the handler but not delivered yet. It
        // changes in the same step that takes the batch out of the state, so release can wait for
        // every one of them, including a batch a timer fiber took but has not consumed yet.
        final case class State(batch: Batch, inFlight: Int)

        if (settings.size <= 1 || settings.delay <= 0.millis) {
          val enqueue: Enqueue[F, A] = a => f(Nel.of(a))
          Resource.pure(enqueue)
        } else {
          val result = for {
            semaphore <- Semaphore[F](1)
            drained <- Deferred[F, Unit]
            ref <- Ref[F].of(State(Batch.empty, 0))
          } yield {

            // The caller counts the batch in while taking it out of the state, so the count is
            // dropped here, once the handler has returned.
            def consume(as: Nel[A]) = {
              val delivered = ref
                .modify { state =>
                  val inFlight = state.inFlight - 1
                  val signal = state.batch match {
                    case Batch.Stopped if inFlight == 0 => drained.complete(()).void
                    case _ => void
                  }
                  (state.copy(inFlight = inFlight), signal)
                }
                .flatten
              semaphore.permit.use { _ => f(as.reverse) }.guarantee(delivered)
            }

            def startTimer(closed: Deferred[F, Unit]) = {
              val expire = ref
                .modify {
                  case State(batch: Batch.Full, inFlight) if batch.closed.eq(closed) =>
                    (State(Batch.empty, inFlight + 1), consume(batch.as))
                  case state => (state, void)
                }
                .flatten
              closed.get
                .timeoutTo(settings.delay, expire)
                .start
                .void
            }

            def append(a: A, state: State, batch: Batch.Full) = {
              val as = a :: batch.as
              val size = batch.size + 1
              if (size >= settings.size) {
                (State(Batch.empty, state.inFlight + 1), batch.closed.complete(()) *> consume(as))
              } else {
                (state.copy(batch = batch.copy(as = as, size = size)), void)
              }
            }

            // An open batch already has its own `closed`, so the allocation happens only on the
            // path that opens one. The state is read again after the allocation, because another
            // fiber can open the batch in between.
            def openBatch(a: A) = {
              for {
                closed <- Deferred[F, Unit]
                action <- ref.modify {
                  case state @ State(Batch.Empty, _) =>
                    (state.copy(batch = Batch.full(a, closed)), startTimer(closed))
                  case state @ State(batch: Batch.Full, _) => append(a, state, batch)
                  case state => (state, void)
                }
                _ <- action
              } yield {}
            }

            val enqueue = new Enqueue[F, A] {

              def apply(a: A) = {
                Concurrent[F].uncancelable { _ =>
                  ref
                    .modify {
                      case state @ State(batch: Batch.Full, _) => append(a, state, batch)
                      case state @ State(Batch.Empty, _) => (state, openBatch(a))
                      case state => (state, void)
                    }
                    .flatten
                }
              }
            }

            val release = {
              val stop = ref
                .modify {
                  case State(batch: Batch.Full, inFlight) =>
                    (State(Batch.stopped, inFlight + 1), batch.closed.complete(()) *> consume(batch.as))
                  case state => (state.copy(batch = Batch.stopped), void)
                }
                .flatten
              // A batch the delay timer already took is delivered by its own fiber, so release
              // waits for every consume that started before it, not only for its own.
              val awaitDelivery = ref.get.flatMap { state => drained.get.whenA(state.inFlight > 0) }
              stop.guarantee(awaitDelivery)
            }

            (enqueue, release)
          }

          Resource(result)
        }
      }
    }
  }

  /**
   * Accepts one element into the current batch.
   */
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
