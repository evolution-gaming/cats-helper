package com.evolutiongaming.catshelper

import cats.effect.Sync
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.LoggingEvent
import ch.qos.logback.classic.util.ContextInitializer
import com.evolutiongaming.catshelper.Log.Mdc

import scala.collection.JavaConverters._

/**
  * Motivation:
  *
  * Direct logback usage required to overcome limitations of SLF4J MDC API.
  * SLF4J MDC API heavily rely on [[ThreadLocal]], example: ch.qos.logback.classic.util.LogbackMDCAdapter
  *
  * Logback' [[LoggingEvent]] allow setting MDC directly as Java map that should have performance benefits compared with SLF4J implementation.
  *
  * Please be aware that defining other version of logback might bring '''RUNTIME ERRORS''' or '''MISSING LOGS''' in case of binary incompatibility between them.
  * Suggested approach is in using exactly same logback version as used in `cats-helper-logback` (among all others available through transitive dependencies)
  */
object Logback {

  def apply[F[_]: Sync]: F[LogOf[F]] =
    Sync[F].delay {
      val context = new ch.qos.logback.classic.LoggerContext()
      new ContextInitializer(context).autoConfig()
      new LogOf[F] {

        def apply(source: String): F[Log[F]] = Sync[F].delay {
          log(context.getLogger(source))
        }

        def apply(source: Class[_]): F[Log[F]] =
          apply(source.getName.stripSuffix("$"))
      }
    }

  private def log[F[_]: Sync](logger: Logger): Log[F] =
    new Log[F] {

      val FQCN = getClass.getName

      def append(msg: => String,
                 mdc: Mdc,
                 level: Level,
                 throwable: Throwable = null): F[Unit] = Sync[F].delay {
        if (logger.isEnabledFor(level)) {
          val event =
            new LoggingEvent(FQCN, logger, level, msg, throwable, null)
          mdc.context.map(_.toSortedMap) foreach { mdc =>
            event.setMDCPropertyMap(mdc.asJava)
          }
          logger.callAppenders(event)
        }
      }

      def trace(msg: => String, mdc: Mdc): F[Unit] =
        append(msg, mdc, Level.TRACE)

      def debug(msg: => String, mdc: Mdc): F[Unit] =
        append(msg, mdc, Level.DEBUG)

      def info(msg: => String, mdc: Mdc): F[Unit] =
        append(msg, mdc, Level.INFO)

      def warn(msg: => String, mdc: Mdc): F[Unit] =
        append(msg, mdc, Level.WARN)

      def warn(msg: => String, cause: Throwable, mdc: Mdc): F[Unit] =
        append(msg, mdc, Level.WARN, cause)

      def error(msg: => String, mdc: Mdc): F[Unit] =
        append(msg, mdc, Level.ERROR)

      def error(msg: => String, cause: Throwable, mdc: Mdc): F[Unit] =
        append(msg, mdc, Level.ERROR, cause)
    }

}
