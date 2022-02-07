package com.evolutiongaming.catshelper

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect.{Ref, Resource, Temporal}

import scala.concurrent.duration.FiniteDuration

/**
  * Type class that aims to provide retry mechanic for [[Resource]].
  * Retrying [[Resource]] means releasing already allocated resources and further reallocation.
  *
  * @tparam F the effect type
  */
trait ResourceRetry[F[_]] {

  def useRetry[A, B](src: Resource[F, A])(use: A => F[B]): F[B]

}

// format: off
object ResourceRetry {

  final case class Config(label: String,
                          attempts: Int,
                          resetAfter: FiniteDuration,
                          retryAfter: FiniteDuration)

  object implicits {

    implicit final class ResourceOps[F[_], A](val src: Resource[F, A]) extends AnyVal {

      def useRetry[B](use: A => F[B])(implicit F: ResourceRetry[F]): F[B] =
        F.useRetry(src)(use)

    }

  }

  def apply[F[_]: ResourceRetry]: ResourceRetry[F] = implicitly

  /**
    * Create [[ResourceRetry]] with configured parameters that include:
    * [[Config.label]] - used in debug logs
    * [[Config.attempts]] - maximum amount of retry attempts before giving up
    * [[Config.resetAfter]] - delay after which used attempts will be set to [[Config.attempts]]
    * [[Config.retryAfter]] - delay before resource reallocation after failure
    *
    * @param config resource retry configuration of type [[Config]]
    * @tparam F the effect type
    * @return [[ResourceRetry]] instance
    */
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
