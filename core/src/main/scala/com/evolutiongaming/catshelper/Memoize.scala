package com.evolutiongaming.catshelper

import cats.effect.kernel.{Deferred, Ref}
import cats.effect.implicits._
import cats.effect.{Concurrent, Sync}
import cats.implicits._


/**
  * Memoize is replacement for `lazy` keyword in cats-effect based code
  */
object Memoize {

  def apply[F[_]: Concurrent]: ApplyBuilders[F] = new ApplyBuilders(Concurrent[F])


  def concurrent[F[_]: Concurrent, A](load: => F[A]): F[F[A]] = {
    Ref[F]
      .of(none[F[A]])
      .map { ref =>
        ref.get.flatMap {
          case Some(a) => a
          case None    =>
            Deferred[F, F[A]].flatMap { deferred =>
              ref
                .modify {
                  case Some(a) => (a.some, a)
                  case None    =>
                    val b = for {
                      a <- load.attempt
                      _ <- deferred.complete(a.liftTo[F])
                      a <- a.liftTo[F]
                    } yield a
                    val a = deferred.get.flatten.some
                    (a, b)
                }
                .flatten
                .uncancelable
            }
        }
      }
  }


  def sync[F[_]: Sync, A](load: => F[A]): F[F[A]] = {
    Ref[F]
      .of(none[F[A]])
      .map { ref =>
        ref.get.flatMap {
          case Some(a) => a
          case None    =>
            for {
              a <- load.attempt
              a <- ref.modify {
                case None    => (a.liftTo[F].some, a.liftTo[F])
                case Some(a) => (a.some, a)
              }
              a <- a
            } yield a
        }
      }
  }


  final class ApplyBuilders[F[_]](val F: Concurrent[F]) extends AnyVal {

    def of[A](load: => F[A]): F[F[A]] = Memoize.concurrent(load)(F)
  }
}
