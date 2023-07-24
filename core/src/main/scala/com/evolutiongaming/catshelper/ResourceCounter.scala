package com.evolutiongaming.catshelper

import cats.effect.{Concurrent, Resource}
import cats.syntax.all._

/** [[ResourceCounter]] is a data structure that allows reusing one [[Resource]] multiple times avoiding re-allocation.
  *
  * Resource allocation is lazy and happened on allocation of [[ResourceCounter.resource]]. If [[ResourceCounter.resource]]
  * allocated again - then the source resource will not be allocated and cached value will be used.
  * In case of concurrent allocation of [[ResourceCounter.resource]] the source resource guaranteed to be allocated only once,
  * same rule applied for releasing resource. The release will happened after last [[ResourceCounter.resource]] is released.
  *
  * [[ResourceCounter]] can be reused multiple times - after is was allocated/released first time one can allocate it again, sample:
  * {{{
  *   val source: Resource[IO, Unit] = ???
  *   for {
  *     rc <- ResourceCounter.of(source)
  *     _ <- rc.resource.use_ // allocated & released
  *     _ <- rc.resource.use_ // allocated & released again
  *     _ <- rc.resource.use_ // allocated & released and again
  *   } yield {}
  * }}}
  *
  * @tparam F the effect type
  * @tparam A the type of resource
  */
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
