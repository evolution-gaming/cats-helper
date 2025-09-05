package com.evolutiongaming.catshelper

import cats.effect.Sync
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.LoggingEvent
import ch.qos.logback.classic.util.ContextInitializer
import com.evolutiongaming.catshelper.Log.Mdc

import scala.jdk.CollectionConverters._
import scala.util.Try

// format: off
/**
  * ===Motivation===
  * Direct logback usage required to overcome limitations of SLF4J MDC API.
  * SLF4J MDC API heavily rely on [[ThreadLocal]], example: ch.qos.logback.classic.util.LogbackMDCAdapter
  *
  * Logback' [[LoggingEvent]] allow setting MDC directly as Java map that should have performance benefits compared with SLF4J/Logback implementation.
  *
  *  ==CAUTION!==
  * Please be aware that using other version of logback (than used in `cats-helper-logback`) might bring '''RUNTIME ERRORS''' or '''MISSING LOGS''' in case of binary incompatibility between them.
  * Suggested approach is in using exactly same logback version as used in `cats-helper-logback` (among all others available through transitive dependencies)
  */
// format: on
object LogOfFromLogback {

  def apply[F[_]: Sync]: F[LogOf[F]] =
    Sync[F].delay {
      // see SLF4J compatibility Readme section
      val slf4jCtx = Try { org.slf4j.LoggerFactory.getILoggerFactory().asInstanceOf[ch.qos.logback.classic.LoggerContext] }
      val context  = slf4jCtx.getOrElse {
        val ctx = new ch.qos.logback.classic.LoggerContext()
        new ContextInitializer(ctx).autoConfig()
        ctx
      }
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

      def append(
          msg: => String,
          mdc: Mdc,
          level: Level,
          throwable: Throwable = null
      ): F[Unit] = Sync[F].delay {
        if (logger.isEnabledFor(level)) {
          val event =
            new LoggingEvent(FQCN, logger, level, msg, throwable, null)
          val mdc1 = mdc.context match {
            case Some(mdc) => mdc.toSortedMap.asJava
            case None      => java.util.Collections.emptyMap[String, String]()
          }
          event.setMDCPropertyMap(mdc1)
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
