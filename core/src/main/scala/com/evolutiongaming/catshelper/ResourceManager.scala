package com.evolutiongaming.catshelper

import cats.syntax.all._
import cats.effect.{Ref, Resource, Sync}

import scala.util.control.NoStackTrace

/**
 * ResourceManager abstracts dynamically allocating resources in scope of one [[Resource]]
 *
 * Example:
 * {{{
 *   def local(): Resource[IO, Unit] = ???
 *
 *   val global = ResourceManager[IO]
 *
 *   val io = global.use { manager =>
 *      for {
 *        l1 <- local()
 *        l2 <- local()
 *        l3 <- local()
 *        // use allocated instances l1, l2, l3
 *      } yield ()
 *   }
 *   // release l1, l2, l3
 * }}}
 */
sealed trait ResourceManager[F[_]] {

  def register[A](resource: Resource[F, A]): F[A]

}

object ResourceManager {

  /** Raised if [[ResourceManager]] used outside of it's Resource scope */
  final case object ResourceManagerAlreadyReleasedException extends RuntimeException with NoStackTrace

  private type Release[F[_]] = F[Unit]

  private sealed trait State[F[_]]
  private object State {
    def empty[F[_]]: State[F] = State.Running[F](List.empty)
    final case class Released[F[_]]() extends State[F]
    final case class Running[F[_]](cache: List[Release[F]]) extends State[F] {
      def + (release: Release[F]): Running[F] = copy(cache :+ release)
    }
  }

  def apply[F[_]: Sync]: Resource[F, ResourceManager[F]] = {
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
        new ResourceManager[F] {

          /**
           * Allocate [[resource]] and remember it's release effect if [[ResourceManager]] still in running state,
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
                  case false => release.attempt *> ResourceManagerAlreadyReleasedException.raiseError
                }
            }
          }

        }
      }
  }

}

