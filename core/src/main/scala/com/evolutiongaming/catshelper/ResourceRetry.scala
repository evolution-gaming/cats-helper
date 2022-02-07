package com.evolutiongaming.catshelper

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect.{Ref, Resource, Temporal}

import scala.concurrent.duration.FiniteDuration

// format: off
trait ResourceRetry[F[_]] {

  def useRetry[A, B](src: Resource[F, A])(use: A => F[B]): F[B]

}

object ResourceRetry {

  final case class Config(label: String,
                          attempts: Int,
                          retryAfter: FiniteDuration,
                          resetAfter: FiniteDuration)

  object implicits {

    implicit final class ResourceOps[F[_], A](val src: Resource[F, A]) extends AnyVal {

      def useRetry[B](use: A => F[B])(implicit F: ResourceRetry[F]): F[B] =
        F.useRetry(src)(use)

    }

  }

  def apply[F[_]: ResourceRetry]: ResourceRetry[F] = implicitly

  def of[F[_]: Temporal: Log](config: Config): ResourceRetry[F] =
    new ResourceRetry[F] {

      def useRetry[A, B](src: Resource[F, A])(use: A => F[B]): F[B] =
        for {
          r <- Ref.of(config.attempts)
          f <- r.set(config.attempts).delayBy(config.resetAfter).foreverM[Unit].start
          b <- run(src, use, r)
          _ <- f.cancel
        } yield b

      def run[A, B](src: Resource[F, A],
                    use: A => F[B],
                    ref: Ref[F, Int]): F[B] = {}.tailRecM { _ =>
        src.allocated.flatMap {
          case (a, release) =>
            use(a).attempt
              .flatTap(_ => release.attempt >> Log[F].debug(s"released resource ${config.label}"))
              .flatMap {
                case Right(b) =>

                  /** `use` completed successfully, exit tailRecM */
                  b.asRight[Unit].pure[F]

                case Left(er) =>
                  ref.updateAndGet(_ - 1).map(_ >= 0).ifM(

                    /** retry `use` with delay, continue tailRecM */
                    ifTrue  = Log[F].debug(s"retry resource ${config.label}").delayBy(config.retryAfter) as ().asLeft,

                    /** no more attempts left, fail with `er` */
                    ifFalse = er.raiseError[F, Either[Unit, B]]
                  )
              }
        }
      }

    }

}
