package com.evolutiongaming.catshelper

import cats.syntax.all._
import cats.effect.{Ref, Resource, Sync}

import scala.util.control.NoStackTrace

/**
 * ResourceRegistry abstracts dynamically allocating resources in scope of one [[Resource]]
 *
 * Example:
 * {{{
 *   trait Service
 *
 *   def make(): Resource[IO, Service] = ???
 *
 *   val global = ResourceRegistry[IO]
 *
 *   val io = global.use { registry =>
 *      for {
 *        s1 <- registry.register(make())
 *        s2 <- registry.register(make())
 *        s3 <- registry.register(make())
 *        // use allocated instances s1, s2, s3
 *      } yield ()
 *   }
 *   // release s1, s2, s3
 * }}}
 */
trait ResourceRegistry[F[_]] {

  def register[A](resource: Resource[F, A]): F[A]

}

object ResourceRegistry {

  /** Raised if [[ResourceRegistry]] used outside of it's Resource scope */
  final case object AlreadyReleasedException extends RuntimeException with NoStackTrace

  private type Release[F[_]] = F[Unit]

  private sealed trait State[F[_]]
  private object State {
    def empty[F[_]]: State[F] = State.Running[F](List.empty)
    final case class Released[F[_]]() extends State[F]
    final case class Running[F[_]](cache: List[Release[F]]) extends State[F] {
      def + (release: Release[F]): Running[F] = copy(cache :+ release)
    }
  }

  def apply[F[_]: Sync]: Resource[F, ResourceRegistry[F]] = {
    val allocate = Ref[F].of(State.empty[F])
    Resource
      .make(allocate) { state =>
        state
          .getAndSet(State.Released[F]())
          .flatMap {
            case running: State.Running[F] => running.cache.reverse.traverse_(_.attempt)
            case _                         => ().pure[F]
          }
      }
      .map { state =>
        new ResourceRegistry[F] {

          /**
           * Allocate [[resource]] and remember it's release effect if [[ResourceRegistry]] still in running state,
           * otherwise release the [[resource]] immediately and raise exception
           */
          def register[A](resource: Resource[F, A]): F[A] = {
            resource.allocated.flatMap {
              case (allocation, release) =>
                state.modify {
                  case running: State.Running[F] => running + release -> true
                  case _                         => State.Released[F]() -> false
                } flatMap {
                  case true  => allocation.pure[F]
                  case false => release.attempt *> AlreadyReleasedException.raiseError
                }
            }
          }

        }
      }
  }

}

