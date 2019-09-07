package com.evolutiongaming.catshelper


import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.implicits._
import cats.implicits._

trait LazyVal[F[_], A] {

  def get: F[A]

  def getLoaded: F[Option[A]]
}

object LazyVal {

  def of[F[_] : Concurrent, A](load: => F[A]): F[LazyVal[F, A]] = {
    for {
      ref <- Ref[F].of(none[Deferred[F, F[A]]])
    } yield {
      apply(ref, load)
    }
  }

  def apply[F[_] : Concurrent, A](
    ref: Ref[F, Option[Deferred[F, F[A]]]],
    load: => F[A]
  ): LazyVal[F, A] = {
    new LazyVal[F, A] {

      def get = {

        def loaded = {

          def complete(d: Deferred[F, F[A]]) = {
            for {
              a  <- load.attempt
              af  = a.raiseOrPure[F]
              _  <- d.complete(af)
            } yield af
          }

          for {
            d <- Deferred[F, F[A]]
            a <- ref.modify {
              case None    => (d.some, complete(d))
              case Some(a) => (a.some, a.get)
            }
            a <- a
          } yield a
        }

        for {
          a <- ref.get
          a <- a.fold(loaded.uncancelable)(_.get)
          a <- a
        } yield a
      }

      def getLoaded = {
        for {
          a <- ref.get
          a <- a.fold(none[A].pure[F])(_.get.flatMap(_.map(_.some)))
        } yield a
      }
    }
  }
}