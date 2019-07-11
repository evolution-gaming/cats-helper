package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.implicits._
import cats.effect.{Clock, Concurrent, Resource, Timer}
import cats.implicits._
import com.evolutiongaming.catshelper.ClockHelper._

import scala.concurrent.duration.FiniteDuration

trait GroupWithin[F[_]] {
  import GroupWithin._

  def apply[A](settings: Settings)(f: Nel[A] => F[Unit]): Resource[F, Enqueue[F, A]]
}

object GroupWithin {

  final case class Settings(delay: FiniteDuration, size: Int)


  def apply[F[_] : Concurrent : Timer]: GroupWithin[F] = {

    new GroupWithin[F] {

      def apply[A](settings: Settings)(f: Nel[A] => F[Unit]) = {

        case class State(as: Nel[A], timestamp: Long, cancel: F[Unit])

        if (settings.size <= 1) {
          val enqueue = new Enqueue[F, A] {
            def apply(a: A) = f(Nel.of(a))
          }
          Resource.liftF(enqueue.pure[F])
        } else {

          val result = for {
            state <- SerialRef[F].of(none[State])
          } yield {

            def consume(as: Nel[A]) = for {
              _ <- f(as.reverse)
            } yield {
              none[State]
            }

            def consumeAndCancel(as: Nel[A], cancel: F[Unit]) = for {
              _ <- cancel
              a <- consume(as)
            } yield a

            def timer(timestamp: Long) = {
              for {
                _ <- Timer[F].sleep(settings.delay)
                _ <- state.update {
                  case None        => none[State].pure[F]
                  case Some(state) =>
                    if (state.timestamp == timestamp) consume(state.as)
                    else state.some.pure[F]
                }
              } yield {}
            }

            def enqueue1(a: A, state: State) = {
              val as = a :: state.as
              if (as.size >= settings.size) {
                consumeAndCancel(as, state.cancel)
              } else {
                state.copy(as = as).some.pure[F]
              }
            }

            def enqueue2(a: A) = {
              for {
                timestamp <- Clock[F].nanos
                fiber     <- timer(timestamp).start
              } yield {
                val as = Nel.of(a)
                val cancel = fiber.cancel
                State(as, timestamp, cancel).some
              }
            }

            val enqueue = new Enqueue[F, A] {

              def apply(a: A) = {
                state.update {
                  case Some(state) => enqueue1(a, state)
                  case None        => enqueue2(a)
                }
              }
            }

            val release = state.update {
              case Some(state) => consumeAndCancel(state.as, state.cancel)
              case None        => none[State].pure[F]
            }

            (enqueue, release)
          }

          Resource(result)
        }
      }
    }
  }


  trait Enqueue[F[_], A] {

    def apply(a: A): F[Unit]
  }
}