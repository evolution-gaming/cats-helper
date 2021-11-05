package com.evolutiongaming.catshelper


import cats.{Functor, ~>}
import cats.effect.Concurrent
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.implicits._
import cats.implicits._

trait LazyVal[F[_], A] {

  def get: F[A]

  def getLoaded: F[Option[A]]
}

object LazyVal {

  def const[F[_] : Functor, A](a: F[A]): LazyVal[F, A] = new LazyVal[F, A] {

    def get = a

    def getLoaded = a.map(_.some)
  }


  def of[F[_] : Concurrent, A](load: => F[A]): F[LazyVal[F, A]] = {
    for {
      ref <- Ref[F].of(none[F[A]])
    } yield {
      apply(ref, load)
    }
  }

  def apply[F[_] : Concurrent, A](
    ref: Ref[F, Option[F[A]]],
    load: => F[A]
  ): LazyVal[F, A] = {
    new LazyVal[F, A] {

      val get = {

        def loadOrJoin: F[A] = {

          def loadOrJoin(d: Deferred[F, Either[Throwable, A]]) = {
            val load1 = for {
              a <- load.attempt
              _ <- d.complete(a)
              a <- a.liftTo[F]
            } yield a
            ref
              .modify {
                case None    => (d.get.rethrow.some, load1)
                case Some(a) => (a.some, a)
              }
              .flatten
              .uncancelable
          }

          for {
            loaded <- Deferred[F, Either[Throwable, A]]
            result <- loadOrJoin(loaded)
          } yield result
        }

        for {
          a <- ref.get
          a <- a.fold(loadOrJoin)(identity)
        } yield a
      }

      val getLoaded = {
        for {
          a <- ref.get
          a <- a.sequence
        } yield a
      }
    }
  }


  implicit class LazyValOps[F[_], A](val self: LazyVal[F, A]) extends AnyVal {

    def mapK[G[_]](f: F ~> G): LazyVal[G, A] = new LazyVal[G, A] {

      def get = f(self.get)

      def getLoaded = f(self.getLoaded)
    }
  }
}