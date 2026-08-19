package com.evolutiongaming.catshelper

import cats.effect.Concurrent
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.implicits._
import cats.{Applicative, Hash}

/**
 * Runs tasks serially per key: tasks with the same key run one after another, in submission order,
 * tasks with different keys run in parallel.
 *
 * As in [[Serial]], the caller does not wait for its turn: `apply` only registers the task, and
 * registration is uncancelable. See [[SerParQueue]] when keyless tasks that order against all keys
 * are needed as well.
 */
trait SerialKey[F[_], -K] {

  /**
   * @return
   *   outer F[_] is about adding `task` to the queue of `key`, inner F[_] is about `task` being
   *   completed. If `task` fails, the inner F[_] raises its error and the queue continues with the
   *   next task.
   */
  def apply[A](key: K)(task: F[A]): F[F[A]]
}

object SerialKey {

  /**
   * No serialization: runs the task in place.
   */
  def empty[F[_]: Applicative, K]: SerialKey[F, K] = new SerialKey[F, K] {
    def apply[A](key: K)(task: F[A]) = task.map { _.pure[F] }
  }

  /**
   * Keys are hash-partitioned into one instance per available core (see [[Partitions]]), to spread
   * contention over several `Ref`s instead of a single one.
   */
  def of[F[_]: Concurrent: Runtime, K: Hash]: F[SerialKey[F, K]] = {
    for {
      cores <- Runtime[F].availableCores
      partitions <- Partitions.of[F, K, SerialKey[F, K]](cores, _ => of1)
    } yield {
      new SerialKey[F, K] {
        def apply[A](key: K)(task: F[A]) = {
          partitions
            .get(key)
            .apply(key)(task)
        }
      }
    }
  }

  /**
   * State per key: `None` - a task runs and nothing is pending, `Some(task)` - a task runs and
   * `task` (pending tasks chained via `productR`) follows it. The entry is removed once the key
   * drains, so the map holds in-flight keys only.
   */
  private def of1[F[_]: Concurrent, K]: F[SerialKey[F, K]] = {

    val void = ().pure[F]

    type Task = F[Unit]

    Ref[F]
      .of(Map.empty[K, Option[Task]])
      .map { ref =>
        def start(key: K, task: Task) = {
          task
            .tailRecM { task =>
              for {
                _ <- task
                a <- ref.modify { map =>
                  map.get(key) match {
                    case Some(Some(a)) => (map.updated(key, none), a.asLeft[Unit])
                    case Some(None) => (map - key, ().asRight[Task])
                    case None => (map, ().asRight[Task])
                  }
                }
              } yield a
            }
            .start
            .void
        }

        new SerialKey[F, K] {
          def apply[A](key: K)(task0: F[A]) = {

            Concurrent[F].uncancelable { _ =>
              for {
                d <- Deferred[F, Either[Throwable, A]]
                task = for {
                  a <- task0.attempt
                  _ <- d.complete(a)
                } yield {}
                a <- ref.modify { map =>
                  map.get(key) match {
                    case None => (map.updated(key, none), start(key, task))
                    case Some(None) => (map.updated(key, task.some), void)
                    case Some(Some(a)) => (map.updated(key, a.productR(task).some), void)
                  }
                }
                _ <- a
              } yield
                for {
                  a <- d.get
                  a <- a.liftTo[F]
                } yield a
            }
          }
        }
      }
  }
}
