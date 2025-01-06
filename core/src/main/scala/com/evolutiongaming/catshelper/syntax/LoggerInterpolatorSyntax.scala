package com.evolutiongaming.catshelper.syntax

import com.evolutiongaming.catshelper.Log

/**
 * Tiny syntax code to log like a string interpolator.
 * e.g. info"App starting.."
 */
object LoggerInterpolatorSyntax {

  implicit final class Interpolator(private val sc: StringContext) extends AnyVal {
    def error[F[_]](message: Any*)(implicit logger: Log[F]): F[Unit] =
      logger.error(sc.s(message: _*))

    def warn[F[_]](message: Any*)(implicit logger: Log[F]): F[Unit] =
      logger.warn(sc.s(message: _*))

    def info[F[_]](message: Any*)(implicit logger: Log[F]): F[Unit] =
      logger.info(sc.s(message: _*))

    def debug[F[_]](message: Any*)(implicit logger: Log[F]): F[Unit] =
      logger.debug(sc.s(message: _*))

    def trace[F[_]](message: Any*)(implicit logger: Log[F]): F[Unit] =
      logger.trace(sc.s(message: _*))
  }
}
