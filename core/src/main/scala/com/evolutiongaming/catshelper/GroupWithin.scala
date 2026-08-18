package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.implicits._
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.std.Semaphore
import cats.effect.{Clock, Concurrent, Resource, Temporal}
import cats.implicits._
import cats.{Applicative, ~>}
import com.evolutiongaming.catshelper.ClockHelper._

import scala.concurrent.duration._

/**
 * Collects elements into batches and passes each batch to a handler.
 *
 * A batch is closed when it reaches [[GroupWithin.Settings.size]] elements, or when
 * [[GroupWithin.Settings.delay]] passes after the first element of the batch, whichever happens
 * first. A pending batch is also closed when the resource is released. Handler calls are
 * serialised, so batches do not overlap, but the order of batches is not guaranteed.
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
   * per element and no state is allocated.
   *
   * While batching, enqueue is uncancelable, so an accepted element is always part of a batch.
   * After release, enqueue silently discards the element.
   */
  def apply[F[_]: Temporal]: GroupWithin[F] = {

    new GroupWithin[F] {

      def apply[A](settings: Settings)(f: Nel[A] => F[Unit]) = {

        val void = ().pure[F]

        sealed trait S

        object S {
          def empty: S = Empty
          def stopped: S = Stopped
          def full(a: A, timestamp: Long, closed: Deferred[F, Unit]): S = Full(Nel.of(a), 1, timestamp, closed)

          case object Empty extends S
          case object Stopped extends S

          /**
           * @param size
           *   number of elements in `as`, counted so that enqueue does not traverse the batch
           * @param closed
           *   completed when the batch is closed by size or by release, which stops its timer
           */
          final case class Full(
            as: Nel[A],
            size: Int,
            timestamp: Long,
            closed: Deferred[F, Unit],
          ) extends S
        }

        if (settings.size <= 1 || settings.delay <= 0.millis) {
          val enqueue: Enqueue[F, A] = a => f(Nel.of(a))
          Resource.pure(enqueue)
        } else {
          val result = for {
            semaphore <- Semaphore[F](1)
            ref <- Ref[F].of(S.empty)
          } yield {

            def consume(as: Nel[A]) = semaphore.permit.use { _ => f(as.reverse) }

            def startTimer(timestamp: Long, closed: Deferred[F, Unit]) = {
              val expire = ref
                .modify {
                  case s: S.Full if s.timestamp == timestamp => (S.empty, consume(s.as))
                  case s => (s, void)
                }
                .flatten
              Temporal[F]
                .race(Temporal[F].sleep(settings.delay), closed.get)
                .flatMap {
                  case Left(_) => expire
                  case Right(_) => void
                }
                .start
                .void
            }

            val enqueue = new Enqueue[F, A] {

              def apply(a: A) = {
                Concurrent[F].uncancelable { _ =>
                  for {
                    timestamp <- Clock[F].nanos
                    closed <- Deferred[F, Unit]
                    action <- ref.modify {
                      case s: S.Full =>
                        val as = a :: s.as
                        val size = s.size + 1
                        if (size >= settings.size) (S.empty, s.closed.complete(()) *> consume(as))
                        else (s.copy(as = as, size = size), void)
                      case S.Empty => (S.full(a, timestamp, closed), startTimer(timestamp, closed))
                      case S.Stopped => (S.stopped, void)
                    }
                    _ <- action
                  } yield {}
                }
              }
            }

            val release = ref
              .modify {
                case s: S.Full => (S.stopped, s.closed.complete(()) *> consume(s.as))
                case _ => (S.stopped, void)
              }
              .flatten

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
