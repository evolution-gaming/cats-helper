package com.evolutiongaming.catshelper

import cats.effect.implicits.effectResourceOps
import cats.effect.kernel.{DeferredSource, RefSource}
import cats.effect.syntax.all.*
import cats.effect.{Deferred, Ref, Resource, Temporal}
import cats.syntax.all.*

import scala.concurrent.duration.*

trait ReloadableRef[F[_], A] extends RefSource[F, A] {
  def tryGet: F[Option[A]]
}

object ReloadableRef {

  def of[F[_]: Temporal, A](
    load: F[A],
    expireIn: FiniteDuration
  ): Resource[F, ReloadableRef[F, A]] = {

    type E = Either[Throwable, A]

    def completed(e: E): DeferredSource[F, E] =
      new DeferredSource[F, E] {
        val get: F[E] = e.pure[F]
        val tryGet: F[Option[E]] = e.some.pure[F]
      }

    for {
      d <- Deferred[F, E].toResource
      r <- Ref.of[F, DeferredSource[F, E]](d).toResource

      f = for {
        e <- load.attempt
        _ <- d.complete(e)
      } yield {}
      _ <- f.start.toResource

      s = for {
        _ <- Temporal[F].sleep(expireIn)
        e <- load.attempt
        _ <- r.set(completed(e))
      } yield {}
      _ <- s.foreverM[Unit].background
    } yield
      new ReloadableRef[F, A] {

        override def get: F[A] =
          for {
            d <- r.get
            e <- d.get
            a <- e.liftTo[F]
          } yield a

        override def tryGet: F[Option[A]] =
          for {
            d <- r.get
            o <- d.tryGet
          } yield
            for {
              e <- o
              a <- e.toOption
            } yield a

      }
  }

}
