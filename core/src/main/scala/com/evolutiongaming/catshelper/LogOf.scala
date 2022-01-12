package com.evolutiongaming.catshelper

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Functor, ~>}
import ch.qos.logback.classic.util.ContextInitializer
import org.slf4j.{ILoggerFactory, LoggerFactory}

import scala.reflect.ClassTag

trait LogOf[F[_]] {

  def apply(source: String): F[Log[F]]

  def apply(source: Class[_]): F[Log[F]]
}

object LogOf {

  def apply[F[_]](implicit F: LogOf[F]): LogOf[F] = F

  def summon[F[_]](implicit F: LogOf[F]): LogOf[F] = F

  def log[F[_]: LogOf, C: ClassTag]: F[Log[F]] = apply[F].apply(implicitly[ClassTag[C]].runtimeClass)

  def log[F[_]: LogOf](name: String): F[Log[F]] = apply[F].apply(name)

  def apply[F[_] : Sync](factory: ILoggerFactory): LogOf[F] = new LogOf[F] {

    def apply(source: String) = {
      for {
        log <- Sync[F].delay { factory.getLogger(source) }
      } yield {
        Log[F](log)
      }
    }
    def apply(source: Class[_]) = apply(source.getName.stripSuffix("$"))
  }


  @deprecated("use `slf4j` instead", "1.0.4")
  def slfj4[F[_] : Sync]: F[LogOf[F]] = slf4j[F]

  def slf4j[F[_] : Sync]: F[LogOf[F]] = {
    for {
      factory <- Sync[F].delay { LoggerFactory.getILoggerFactory }
    } yield {
      apply(factory)
    }
  }

  def logback[F[_] : Sync]: F[LogOf[F]] =
    for {
      context <- Sync[F].delay { new ch.qos.logback.classic.LoggerContext() }
      _ = new ContextInitializer(context).autoConfig()
    } yield new LogOf[F] {

      def apply(source: String): F[Log[F]] = Sync[F].delay { Log(context.getLogger(source)) }

      def apply(source: Class[_]): F[Log[F]] = apply(source.getName.stripSuffix("$"))
    }


  def empty[F[_] : Applicative]: LogOf[F] = const(Log.empty[F].pure[F])


  def const[F[_]](log: F[Log[F]]): LogOf[F] = new LogOf[F] {

    def apply(source: String) = log

    def apply(source: Class[_]) = log
  }


  implicit class LogOfOps[F[_]](val self: LogOf[F]) extends AnyVal {

    def mapK[G[_] : Functor](f: F ~> G): LogOf[G] = new LogOf[G] {

      def apply(source: String) = {
        for {
          log <- f(self(source))
        } yield {
          log.mapK(f)
        }
      }

      def apply(source: Class[_]) = {
        for {
          log <- f(self(source))
        } yield {
          log.mapK(f)
        }
      }
    }
  }
}