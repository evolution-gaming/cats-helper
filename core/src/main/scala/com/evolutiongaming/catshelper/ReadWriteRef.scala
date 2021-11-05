package com.evolutiongaming.catshelper

import cats.Applicative
import cats.effect.{Concurrent, Resource}
import cats.effect.kernel.{Deferred, Ref}
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
 *
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
    /* Just a stable `val` that holds `F.unit`. Used in some optimizations here. */
    val FUnit: F[Unit] = F.unit

    /**
     * Represents a pending operation on inner `A` reference.
     * Each operation has a "version" `v` that's used by completion/cancellation callbacks.
     * Each operation also has a `trigger` effect that starts pending operations.
     */
    sealed trait Pending {
      def v: Long
    }
    object Pending {
      /**
       * Represents a "read" operation. Multiple reads can run in parallel.
       * @param active a number of not-yet-complete operations. Grows when new reads get added to this batch.
       *               Decreases when operations complete. `0` means that the batch can be discarded.
       * @param await completes when `trigger` is activated. Used to attach new operations to the same trigger.
       */
      final case class Read(v: Long, trigger: F[Unit], active: Int, await: F[Unit]) extends Pending {
        def incActive: Read = copy(active = active + 1)
        def decActive: Read = copy(active = active - 1)
      }

      /**
       * Represents a "write" operation. Writes are exclusive.
       * @param nextRead A "read" batch that shall be executed after this write. Can be empty.
       *                 This field effectively guarantees that non-empty queue of `Pending` items
       *                 always has a `Read` as the last element.
       */
      final case class Write(v: Long, trigger: F[Unit], nextRead: Read) extends Pending
    }

    /* Holds a queue of `Pending` items. The head, if such exists, is considered "active". */
    case class State(pending: Queue[Pending] = Queue.empty, v: Long = 0) {
      self =>

      def withoutRead(v: Long): State = {
        updateIndex(v) {
          case r: Pending.Read  => if (r.active > 1) Some(r.decActive) else None
          case w: Pending.Write => Some(w.copy(nextRead = w.nextRead.decActive))
        }
      }

      def withoutWrite(v: Long): State = {
        updateIndex(v) {
          case w: Pending.Write => if (w.nextRead.active > 0) Some(w.nextRead) else None
          case unchanged        => Some(unchanged)
        }
      }

      private def updateIndex(v: Long)(f: Pending => Option[Pending]): State = {
        // Versions grow monotonically, so we can stop as soon as we find one or go beyond it
        // Note that we don't account for the overflow yet. But even with increments happening
        // each nanosecond we have 292 years to hit it.
        @tailrec def loop(head: Queue[Pending], q: Queue[Pending]): State = {
          q match {
            case (p: Pending) +: tail if p.v < v  => loop(head :+ p, tail)
            case (p: Pending) +: tail if p.v == v => self.copy(pending = head ++: (f(p) ++: tail))
            case _                                => self
          }
        }
        loop(Queue.empty, self.pending)
      }
    }

    (Ref[F].of(init), Ref[F].of(State())) mapN { (aRef, stateRef) =>
      val upd: Upd[F, A] = (f: A => F[A]) => aRef.get
        .flatMap(f)
        .flatTap(aRef.set)

      def release(stateMod: State => State): F[Unit] = {
        stateRef
          .modify { s =>
            val s0 = stateMod(s)
            s0.pending match {
              case (w: Pending.Write) +: tail =>
                val s1 =
                  if (w.trigger == FUnit) s0
                  else s0.copy(pending = w.copy(trigger = FUnit) +: tail)
                s1 -> w.trigger

              case q =>
                @tailrec def loop(head: Queue[Pending.Read], q: Queue[Pending], effect: F[Unit]): (State, F[Unit]) = {
                  q match {
                    case (r: Pending.Read) +: tail =>
                      r.trigger match {
                        case FUnit   => loop(head :+ r, tail, effect)
                        case trigger => loop(head :+ r.copy(trigger = FUnit), tail, effect *> trigger)
                      }

                    case _ =>
                      s0.copy(pending = head ++: q) -> effect
                  }
                }

                loop(Queue.empty, q, FUnit)
            }
          }
          .flatten
      }

      val read: Resource[F, A] = {
        for {
          access <- Resource {
            stateRef.modify { s0 =>
              val (s1, r1) = s0.pending match {
                case init :+ (r: Pending.Read) =>
                  val r1 = r.incActive
                  val s1 = s0.copy(pending = init :+ r1)
                  (s1, r1)

                case init :+ (w: Pending.Write) =>
                  val r1 = w.nextRead.incActive
                  val w1 = w.copy(nextRead = r1)
                  val s1 = s0.copy(pending = init :+ w1)
                  (s1, r1)

                case _ =>
                  val r1 = Pending.Read(s0.v, trigger = FUnit, active = 1, await = FUnit)
                  val s1 = s0.copy(pending = Queue(r1), v = s0.v + 1)
                  (s1, r1)
              }

              val access = r1.await *> aRef.get
              s1 -> (access -> release(_.withoutRead(r1.v)))
            }
          }
          a <- access.toResource
        } yield a

      }

      val write: Resource[F, Upd[F, A]] = {
        for {
          wTrigger <- Deferred[F, Unit].toResource
          rTrigger <- Deferred[F, Unit].toResource
          access   <- Resource {
            stateRef.modify { s0 =>
              val (writeTrigger, writeAwait) =
                if (s0.pending.isEmpty) FUnit -> FUnit
                else wTrigger.complete(()).start.void -> wTrigger.get

              val w1 = Pending.Write(
                s0.v,
                trigger = writeTrigger,
                nextRead = Pending.Read(
                  s0.v,
                  trigger = rTrigger.complete(()).start.void,
                  active = 0,
                  rTrigger.get,
                ),
              )

              val s1 = s0.copy(s0.pending :+ w1, s0.v + 1)
              val access = writeAwait as upd
              s1 -> (access -> release(_.withoutWrite(w1.v)))
            }
          }
          upd <- access.toResource
        } yield upd
      }

      Impl(read, write)
    }
  }

  private final case class Impl[F[_], A](read: Resource[F, A], write: Resource[F, ReadWriteRef.Upd[F, A]])
    extends ReadWriteRef[F, A]
}
