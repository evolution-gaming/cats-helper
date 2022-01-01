package com.evolutiongaming.catshelper

import cats.effect.Concurrent
import cats.effect.syntax.all._
import cats.implicits._
import cats.{Applicative, Hash, Parallel}
import cats.effect.{ Deferred, Ref }

trait SerialKey[F[_], -K] {

  def apply[A](key: K)(task: F[A]): F[F[A]]
}

object SerialKey {

  def empty[F[_]: Applicative, K]: SerialKey[F, K] = new SerialKey[F, K] {
    def apply[A](key: K)(task: F[A]) = task.map { _.pure[F] }
  }

  def of[F[_]: Concurrent: Parallel: Runtime, K: Hash]: F[SerialKey[F, K]] = {
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
          def apply[A](key: K)(task: F[A]) = {

            val result = for {
              d <- Deferred[F, Either[Throwable, A]]
              a  = for {
                a <- task.attempt
                a <- d.complete(a)
              } yield a
              a <- ref.modify { map =>
                map.get(key) match {
                  case None          => (map.updated(key, none), start(key, a))
                  case Some(None)    => (map.updated(key, a.some), void)
                  case Some(Some(b)) => (map.updated(key, b.productR(a).some), void)
                }
              }
              _ <- a
            } yield for {
              a <- d.get
              a <- a.liftTo[F]
            } yield a

            result.uncancelable
          }
        }
      }
  }
}