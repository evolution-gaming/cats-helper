package com.evolutiongaming.catshelper

import cats.effect.Sync
import cats.{Applicative, ~>}
import org.slf4j.Logger

trait Log[F[_]] {

  def debug(msg: => String): F[Unit]

  def info(msg: => String): F[Unit]

  def warn(msg: => String): F[Unit]

  def warn(msg: => String, cause: Throwable): F[Unit]

  def error(msg: => String): F[Unit]

  def error(msg: => String, cause: Throwable): F[Unit]
}

object Log {

  def apply[F[_]](implicit F: Log[F]): Log[F] = F

  def summon[F[_]](implicit F: Log[F]): Log[F] = F


  def apply[F[_] : Sync](logger: Logger): Log[F] = new Log[F] {

    def debug(msg: => String) = {
      Sync[F].delay {
        if (logger.isDebugEnabled) logger.debug(msg)
      }
    }

    def info(msg: => String) = {
      Sync[F].delay {
        if (logger.isInfoEnabled) logger.info(msg)
      }
    }

    def warn(msg: => String) = {
      Sync[F].delay {
        if (logger.isWarnEnabled) logger.warn(msg)
      }
    }

    def warn(msg: => String, cause: Throwable) = {
      Sync[F].delay {
        if (logger.isWarnEnabled) logger.warn(msg, cause)
      }
    }

    def error(msg: => String) = {
      Sync[F].delay {
        if (logger.isErrorEnabled) logger.error(msg)
      }
    }

    def error(msg: => String, cause: Throwable) = {
      Sync[F].delay {
        if (logger.isErrorEnabled) logger.error(msg, cause)
      }
    }
  }


  def const[F[_]](unit: F[Unit]): Log[F] = new Log[F] {

    def debug(msg: => String) = unit

    def info(msg: => String) = unit

    def warn(msg: => String) = unit

    def warn(msg: => String, cause: Throwable) = unit

    def error(msg: => String) = unit

    def error(msg: => String, cause: Throwable) = unit
  }


  def empty[F[_] : Applicative]: Log[F] = const(Applicative[F].unit)


  implicit class LogOps[F[_]](val self: Log[F]) extends AnyVal {

    def mapK[G[_]](f: F ~> G): Log[G] = new Log[G] {

      def debug(msg: => String) = f(self.debug(msg))

      def info(msg: => String) = f(self.info(msg))

      def warn(msg: => String) = f(self.warn(msg))

      def warn(msg: => String, cause: Throwable) = f(self.warn(msg, cause))

      def error(msg: => String) = f(self.error(msg))

      def error(msg: => String, cause: Throwable) = f(self.error(msg, cause))
    }


    def mapMsg(f: String => String): Log[F] = new Log[F] {

      def debug(msg: => String) = self.debug(f(msg))

      def info(msg: => String) = self.info(f(msg))

      def warn(msg: => String) = self.warn(f(msg))

      def warn(msg: => String, cause: Throwable) = self.warn(f(msg), cause)

      def error(msg: => String) = self.error(f(msg))

      def error(msg: => String, cause: Throwable) = self.error(f(msg), cause)
    }


    def prefixed(prefix: String): Log[F] = mapMsg(msg => s"$prefix $msg")
  }
}