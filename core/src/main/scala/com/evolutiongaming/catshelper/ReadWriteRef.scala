package com.evolutiongaming.catshelper

import cats.Applicative
import cats.effect.{Concurrent, Resource}
import cats.effect.concurrent.Ref
import cats.implicits._

/**
 * A mutable reference to `A` value with read-write lock semantics:
 *  - multiple "read" operations are allowed as long as there's no "write" running.
 *  - "write" operation is exclusive.
 *  - NB: "read" can't be promoted to "write" yet. Attempting a "write" inside "read" will deadlock.
 *
 * Build it with [[ReadWriteRef.PartialApply.of `ReadWriteRef[F].of(a)`]] or
 * [[ReadWriteRef.of `ReadWriteRef.of[F, A](a)`]].
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
    case class State(
      a: A,
      inUse: Int = 0, // -1 -- exclusive write, 0 -- not in use, >0 -- readers
      pendingReads: List[A => F[Unit]] = List.empty,
      pendingWrites: List[A => F[Unit]] = List.empty,
    )

    Ref[F].of(State(init)) map { stateRef =>
      // The `release` function "commits" A back to the state, adjusts `inUse` count,
      // and notifies pending listeners.
      //
      // IMPORTANT:
      //   This function must be called only from uncancellable blocks (e.g. Resource release).
      def release(a: A, useChange: Int => Int): F[Unit] = stateRef
        .modify { s0 =>
          val inUse = useChange(s0.inUse)

          s0.pendingWrites match {
            // Give writers a priority
            case writeListener :: rest if inUse == 0 =>
              val s1 = s0.copy(a = a, inUse = inUse, pendingWrites = rest)
              val effect = writeListener(a)
              s1 -> effect

            case _ =>
              val s1 = s0.copy(a = a, inUse = inUse, pendingReads = List.empty)
              val effect = s0.pendingReads.traverse_(l => l(a))
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
                case s0 if s0.inUse >= 0 => // can read
                  val s1 = s0.copy(inUse = s0.inUse + 1)
                  val effect = listener(s0.a)
                  val cancel = F.unit
                  s1 -> (effect as cancel)

                case s0 => // have to wait for writer
                  val s1 = s0.copy(pendingReads = listener :: s0.pendingReads)
                  val effect = F.unit
                  val cancel = stateRef.update(s => s.copy(pendingReads = s.pendingReads.filterNot(_ eq listener)))
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
                case s0 if s0.inUse == 0 => // can write
                  val s1 = s0.copy(inUse = -1)
                  val effect = listener(s0.a)
                  val cancel = F.unit
                  s1 -> (effect as cancel)

                case s0 => // can't write
                  val s1 = s0.copy(pendingWrites = listener :: s0.pendingWrites)
                  val effect = F.unit
                  val cancel = stateRef.update(s => s.copy(pendingWrites = s.pendingWrites.filterNot(_ eq listener)))
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
