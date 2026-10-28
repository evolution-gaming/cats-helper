package com.evolutiongaming.catshelper

import cats.effect.kernel.Sync

/** Capability of logging messages using a given [[Logger]] instance */
trait Logging[F[_]] {

  def debug(logger: Logger, message: String): F[Unit]
  def info(logger: Logger, message: String): F[Unit]
  def warn(logger: Logger, message: String): F[Unit]
  def error(logger: Logger, message: String): F[Unit]

}

object Logging {

  def apply[F[_]](implicit F: Logging[F]): Logging[F] = F
  
  def fromSync[F[_]: Sync]: Logging[F] = new Logging[F] {
    def debug(logger: Logger, message: String): F[Unit] =
      Sync[F].delay(logger.unsafe.debug(message))
    def info(logger: Logger, message: String): F[Unit] =
      Sync[F].delay(logger.unsafe.info(message))    
    def warn(logger: Logger, message: String): F[Unit] =
      Sync[F].delay(logger.unsafe.warn(message))
    def error(logger: Logger, message: String): F[Unit] =
      Sync[F].delay(logger.unsafe.error(message))
  }

}