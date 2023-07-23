package com.evolutiongaming.catshelper

import cats.effect.{Concurrent, Resource}
import cats.syntax.all.*

trait ResourceCounter[F[_], A] {
  def resource: Resource[F, A]
}

object ResourceCounter {

  def of[F[_]: Concurrent, A](
    source: Resource[F, A]
  ): F[ResourceCounter[F, A]] = {

    trait State
    object State {
      case class Allocated(a: A, release: F[Unit], count: Int) extends State
      object Released extends State
    }

    for {
      state <- SerialRef.of[F, State](State.Released)
    } yield
      new ResourceCounter[F, A] {

        override def resource: Resource[F, A] = {

          val acquire =
            state.modify {

              case State.Released =>
                source.allocated.map {
                  case (a, r) =>
                    val s: State = State.Allocated(a, r, 1)
                    s -> a
                }

              case State.Allocated(a, r, c) =>
                val s: State = State.Allocated(a, r, c + 1)
                (s -> a).pure[F]
            }

          val cleanup =
            state.update {

              case State.Released =>
                val msg = "releasing non-acquired ResourceCounter"
                new IllegalStateException(msg).raiseError[F, State]

              case State.Allocated(a, r, c) =>
                if (c <= 1) {
                  r.as[State](State.Released)
                } else {
                  val s: State = State.Allocated(a, r, c - 1)
                  s.pure[F]
                }
            }

          Resource.make(acquire)(_ => cleanup)
        }

      }
  }

}
