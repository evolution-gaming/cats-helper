package com.evolutiongaming.catshelper

import cats.effect.Concurrent
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.implicits._
import cats.{Applicative, Hash}

trait SerialKey[F[_], -K] {

  def apply[A](key: K)(task: F[A]): F[F[A]]
}

object SerialKey {

  def empty[F[_]: Applicative, K]: SerialKey[F, K] = new SerialKey[F, K] {
    def apply[A](key: K)(task: F[A]) = task.map { _.pure[F] }
  }

  def of[F[_]: Concurrent: Runtime, K: Hash]: F[SerialKey[F, K]] = {
    for {
      cores      <- Runtime[F].availableCores
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
                    case Some(None)    => (map - key, ().asRight[Task])
                    case None          => (map, ().asRight[Task])
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
                d <- Deferred.apply[F, Either[Throwable, A]]
                task = for {
                  a <- task0.attempt
                  _ <- d.complete(a)
                } yield {}
                a <- ref.modify { map =>
                  map.get(key) match {
                    case None          => (map.updated(key, none), start(key, task))
                    case Some(None)    => (map.updated(key, task.some), void)
                    case Some(Some(a)) => (map.updated(key, a.productR(task).some), void)
                  }
                }
                _ <- a
              } yield for {
                a <- d.get
                a <- a.liftTo[F]
              } yield a
            }
          }
        }
      }
  }
}