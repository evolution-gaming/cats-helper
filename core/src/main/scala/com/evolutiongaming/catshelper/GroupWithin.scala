package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.effect.implicits._
import cats.effect.{Clock, Concurrent, Resource, Temporal}
import cats.implicits._
import cats.{Applicative, ~>}
import com.evolutiongaming.catshelper.ClockHelper._

import scala.concurrent.duration._

trait GroupWithin[F[_]] {
  import GroupWithin._

  def apply[A](settings: Settings)(f: Nel[A] => F[Unit]): Resource[F, Enqueue[F, A]]
}

object GroupWithin {

  final case class Settings(delay: FiniteDuration, size: Int)


  def empty[F[_]]: GroupWithin[F] = new GroupWithin[F] {

    def apply[A](settings: Settings)(f: Nel[A] => F[Unit]): Resource[F, Enqueue[F, A]] = {
      val enqueue = new Enqueue[F, A] {
        def apply(a: A) = f(Nel.of(a))
      }
      Resource.pure(enqueue)
    }
  }


  def apply[F[_]: Temporal]: GroupWithin[F] = {

    new GroupWithin[F] {

      def apply[A](settings: Settings)(f: Nel[A] => F[Unit]) = {

        val void = ().pure[F]

        sealed trait S

        object S {
          def empty: S = Empty
          def stopped: S = Stopped
          def full(as: Nel[A], timestamp: Long): S = Full(as, timestamp)

          case object Empty extends S
          case object Stopped extends S
          final case class Full(as: Nel[A], timestamp: Long) extends S
        }

        if (settings.size <= 1 || settings.delay <= 0.millis) {
          val enqueue: Enqueue[F, A] = a => f(Nel.of(a))
          Resource.pure(enqueue)
        } else {
          val result = for {
            semaphore <- Semaphore[F](1)
            ref       <- Ref[F].of(S.empty)
          } yield {

            def consume(as: Nel[A]) = semaphore.permit.use { _ => f(as.reverse) }

            def startTimer(timestamp: Long) = {
              val result = for {
                _ <- Temporal[F].sleep(settings.delay)
                a <- ref.modify {
                  case s: S.Full if s.timestamp == timestamp => (S.empty, consume(s.as))
                  case s                                     => (s, void)
                }
                a <- a
              } yield a
              result
                .start
                .void
            }

            val enqueue = new Enqueue[F, A] {

              def apply(a: A) = {
                Concurrent[F].uncancelable { _ =>
                  for {
                    t <- Clock[F].nanos
                    a <- ref.modify {
                      case s: S.Full =>
                        val as = a :: s.as
                        if (as.size >= settings.size) (S.empty, consume(as))
                        else (s.copy(as = as), void)
                      case S.Empty   => (S.full(Nel.of(a), t), startTimer(t))
                      case S.Stopped => (S.stopped, void)
                    }
                    a <- a
                  } yield a
                }
              }
            }

            val release = ref
              .modify {
                case s: S.Full => (S.stopped, consume(s.as))
                case _         => (S.stopped, void)
              }
              .flatten

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

  object Enqueue {

    def empty[F[_]: Applicative, A]: Enqueue[F, A] = const[F, A](().pure[F])


    def const[F[_], A](value: F[Unit]): Enqueue[F, A] = _ => value


    implicit class EnqueueOps[F[_], A](val self: Enqueue[F, A]) extends AnyVal {

      def mapK[G[_]](f: F ~> G): Enqueue[G, A] = (a: A) => f(self(a))
    }
  }
}