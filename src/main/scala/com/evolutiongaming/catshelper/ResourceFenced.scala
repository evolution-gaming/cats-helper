package com.evolutiongaming.catshelper

/**
 * Ensures release called at most once no matter what
 */
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.implicits._
import cats.effect.{Concurrent, Resource}
import cats.implicits._

object ResourceFenced {

  def apply[F[_] : Concurrent, A](resource: Resource[F, A]): Resource[F, A] = {

    val result = for {
      ref <- Ref[F].of(none[F[Unit]])
      ab  <- resource.allocated
    } yield {
      val (a, release) = ab

      def releaseOrJoin(released: Deferred[F, Either[Throwable, Unit]]) = {
        val release1 = for {
          a <- release.attempt
          _ <- released.complete(a)
        } yield {}

        ref
          .modify {
            case None    => (released.get.rethrow.some, release1)
            case Some(a) => (a.some, a)
          }
          .flatten
          .uncancelable
      }

      val release1 = for {
        released <- Deferred[F, Either[Throwable, Unit]]
        result   <- releaseOrJoin(released)
      } yield result

      (a, release1.uncancelable)
    }

    Resource(result.uncancelable)
  }
}