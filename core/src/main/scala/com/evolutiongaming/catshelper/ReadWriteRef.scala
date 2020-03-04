package com.evolutiongaming.catshelper

import cats.Applicative
import cats.effect.{Concurrent, Resource}
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * A mutable reference to `A` value with read-write lock semantics:
 *  - multiple "read" operations are allowed as long as there's no "write" running or pending.
 *  - "write" operation is exclusive.
 *
 * Build it with [[ReadWriteRef.PartialApply.of `ReadWriteRef[F].of(a)`]] or
 * [[ReadWriteRef.of `ReadWriteRef.of[F, A](a)`]].
 *
 * IMPORTANT: Nested operations may ``block indefinitely`` when used to produce a result for
 * the outer scope. Not using a result of an inner operation (e.g. forking a fiber) will not block.
 *
 * @example This will block {{{
 *   rw.read.use(_ => rw.write.use(…))
 * }}}
 *
 * Cases that will currently block:
 *  - `write` in `read` or `write`.
 *  - `read`  in `read` or `write` when there is another `write` pending.

 * @note Both [[ReadWriteRef.read `read`]] and [[ReadWriteRef.write `write`]] are implemented as `Resource`
 *       since it has clear mental model of something that may be "in use".
 */
trait ReadWriteRef[F[_], A] {

  /**
   * "Read" operation. Multiple reads may be in use simultaneously.
   * `reads.use(…)` blocks semantically while [[write]] in in use.
   */
  def read: Resource[F, A]

  /**
   * "Write" operation. Requires exclusive access.
   * `write.use(…)` blocks semantically while [[read]] or another [[write]] is in use.
   */
  def write: Resource[F, ReadWriteRef.Upd[F, A]]
}

object ReadWriteRef {
  /**
   * A "resource" of [[ReadWriteRef.write `write`]].
   * `(A => F[A])` part lets you read current value and produce a new one, effectfully.
   * `F[A]` in the return part lets you use new `A` in the outer context, e.g. return it from `use`.
   */
  trait Upd[F[_], A] extends ((A => F[A]) => F[A])

  object Upd {
    implicit final class SelfOps[F[_], A](val self: Upd[F, A]) extends AnyVal {
      /** A shorthand for non-effectful updates. */
      def plain(f: A => A)(implicit F: Applicative[F]): F[A] = self(a => F.pure(f(a)))

      /** A shorthand for non-effectful update that ignores previous value. */
      def set(a: A)(implicit F: Applicative[F]): F[A] = self(_ => F.pure(a))
    }
  }

  final class PartialApply[F[_]: Concurrent] {
    def of[A](init: A): F[ReadWriteRef[F, A]] = ReadWriteRef.of[F, A](init)
  }

  def apply[F[_]: Concurrent]: PartialApply[F] = new PartialApply[F]

  def of[F[_], A](init: A)(implicit F: Concurrent[F]): F[ReadWriteRef[F, A]] = {
    sealed trait Pending
    case class PendingRead(listener: A => F[Unit]) extends Pending
    case class PendingWrite(listener: A => F[Unit]) extends Pending

    case class State(
      a: A,
      inUse: Int = 0, // -1 -- exclusive write, 0 -- not in use, >0 -- readers
      pending: Queue[Pending] = Queue.empty,
    ) {
      def canRead: Boolean = inUse >= 0 && pending.isEmpty
      def canWrite: Boolean = inUse == 0 && pending.isEmpty

      def without(p: Pending): State = {
        // Might want to optimise this later.
        copy(pending = pending.filterNot(_ == p))
      }
    }

    Ref[F].of(State(init)) map { stateRef =>
      // The `release` function "commits" A back to the state, adjusts `inUse` count,
      // and notifies pending listeners.
      //
      // IMPORTANT:
      //   This function must be called only from uncancellable blocks (e.g. Resource release).
      def release(a: A, useChange: Int => Int): F[Unit] = stateRef
        .modify { s0 =>
          val inUse = useChange(s0.inUse)

          s0.pending match {
            case PendingWrite(listener) +: rest if inUse == 0 =>
              val s1 = s0.copy(a = a, inUse = -1, pending = rest)
              val effect = listener(a).start.void
              s1 -> effect

            case _ =>
              // A slightly optimized way to split the pending ops in two and start the first batch
              @tailrec def startPending(effect: F[Unit], n: Int, ps: Queue[Pending]): (F[Unit], Int, Queue[Pending]) = {
                ps match {
                  case PendingRead(listener) +: tail => startPending(effect <* listener(a).start, n + 1, tail)
                  case _                             => (effect, n, ps)
                }
              }
              val (effect, nStarted, rest) = startPending(F.unit, 0, s0.pending)
              val s1 = s0.copy(a = a, inUse = inUse + nStarted, pending = rest)
              s1 -> effect
          }
        }
        .flatten

      val read: Resource[F, A] = {
        // `suspend` makes it possible to cancel `.use` that waits for resource availability.
        Resource.suspend {
          Concurrent.cancelableF[F, Resource[F, A]] { cb =>
            // Listener emits a resource when it's ready for immediate use.
            val listener = (a: A) => {
              val r = Resource { F.pure(a -> release(a, _ - 1)) }
              F.delay(cb(r.asRight))
            }

            stateRef
              .modify {
                case s0 if s0.canRead =>
                  val s1 = s0.copy(inUse = s0.inUse + 1)
                  val effect = listener(s0.a)
                  val cancel = F.unit
                  s1 -> (effect as cancel)

                case s0 => // have to wait for writer
                  val pending = PendingRead(listener)
                  val s1 = s0.copy(pending = s0.pending :+ pending)
                  val effect = F.unit
                  val cancel = stateRef.update(_.without(pending))
                  s1 -> (effect as cancel)
              }
              .flatten
          }
        }
      }

      val write: Resource[F, Upd[F, A]] = {
        Resource.suspend {
          Concurrent.cancelableF[F, Resource[F, Upd[F, A]]] { cb =>
            val listener = (a: A) => {
              val r = Resource {
                Ref[F].of(a) map { aRef =>
                  val upd: Upd[F, A] = (f: A => F[A]) => aRef.get.flatMap(f).flatTap(aRef.set)
                  val commit = aRef.get.flatMap(a => release(a, _ => 0))
                  upd -> commit
                }
              }
              F.delay(cb(r.asRight))
            }

            stateRef
              .modify {
                case s0 if s0.canWrite =>
                  val s1 = s0.copy(inUse = -1)
                  val effect = listener(s0.a)
                  val cancel = F.unit
                  s1 -> (effect as cancel)

                case s0 => // can't write
                  val pending = PendingWrite(listener)
                  val s1 = s0.copy(pending = s0.pending :+ pending)
                  val effect = F.unit
                  val cancel = stateRef.update(_.without(pending))
                  s1 -> (effect as cancel)
              }
              .flatten
          }
        }
      }

      Impl(read, write)
    }
  }

  final case class Impl[F[_], A](read: Resource[F, A], write: Resource[F, ReadWriteRef.Upd[F, A]])
    extends ReadWriteRef[F, A]
}
