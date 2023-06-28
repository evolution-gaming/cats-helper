package com.evolution.catshelper

/**
 * Ensures release called at most once no matter what
 */
import cats.effect.implicits._
import cats.effect.{Concurrent, Resource}
import cats.implicits._

object ResourceFenced {

  def apply[F[_] : Concurrent, A](resource: Resource[F, A]): Resource[F, A] = {

    val result = for {
      ab           <- resource.allocated
      (a, release)  = ab
      released     <- LazyVal.of(release)
    } yield {
      (a, released.get)
    }

    Resource(result.uncancelable)
  }
}