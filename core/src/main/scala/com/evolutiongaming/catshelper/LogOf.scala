package com.evolutiongaming.catshelper

import cats.effect.Sync
import cats.effect.std.Console
import cats.implicits._
import cats.{Applicative, Monad, Functor, ~>}
import org.slf4j.{ILoggerFactory, LoggerFactory}

import scala.reflect.ClassTag

/** Factory of [[Log]] instances.
  * 
  * There are, currently, two ways of using the class and it is recommended to
  * select one, describe it in project coding guidelines, and stick with it
  * to avoid a confusion for the newcomers. Both ways are described below.
  * 
  * =Traditional approach=
  * 
  * The intented usage is to have a single instance of `LogOf` in the
  * application and use it to create `Log` instances for each class.
  *
  * The following could be written somewhere in the application initialization
  * code such as [[cats.effect.IOApp]] instance:
  * {{{
  * implicit val logOf = LogOf.slf4j[F]
  * }}}
  * 
  * Then a typical example would be:
  * {{{
  * object UserService {
  * 
  *   def of[F[_]: LogOf]: F[UserService[F]] =
  *     for {
  *       log <- LogOf[F].forClass[UserService]
  *       service = new UserService[F](log)
  *     } yield service
  * 
  * }
  * 
  * class UserService[F[_]: Monad](log: Log[F]) {
  * 
  *  def create(user: User): F[Unit] =
  *    for {
  *      _ <- log.info(s"Creating user...")
  *      _ <- ...
  *    } yield ()
  *   
  * }
  * }}}
  * 
  * The expected output, depending on the logging configuration, would be:
  * {{{
  * [2025-01-13T12:24:23.0Z] INFO UserService - Creating user...
  * }}}
  * 
  * The main advantage of such approach is a minimal boilerplate when passing
  * `LogOf` around, and an ability to reuse `LogOf` and `Log` instances,
  * decreasing the performance overhead.
  * 
  * =MDC-style approach=
  * 
  * The alternative way to use `LogOf` is to create a child instance each time
  * the additional information should be passed to the underlying call, making
  * it a poor's man MDC. The additonial information is then to be passed using
  * `withField` method on `LogOf` itself.
  * 
  * If `LogOf` is used in such a way, it should not be passed implicitly, to
  * avoid accidential mix up of the `LogOf` instances.
  * 
  * For sake of uniformity, if this approach is used, the recommended practice
  * is to never pass `Log` instance anywhere at all, creating a new one each
  * time it is required. Saying that, it is acceptable to reuse them for
  * performance or brevity reasons.
  * 
  * A typical example of such usage could be following:
  * {{{
  * class HandleService[F[_]: Monad] {
  *
  *   def create(logOf: LogOf[F]): F[String] =
  *     for {
  *       log <- logOf.forClass[HandleService]
  *       _   <- log.info(s"Creating handle...")
  *       _   <- ...
  *     } yield ()
  * 
  * }
  * 
  * class UserService[F[_]: Monad](handleService: HandleService[F]) {
  *
  *   def create(user: User, logOf: LogOf[F]): F[Unit] =
  *     for {
  *       log    <- logOf.forClass[UserService]
  *       _      <- log.info(s"Creating user...")
  *       handle <- handleService.create(logOf.withField("user", user.id))
  *       _      <- ...
  *     } yield ()
  * 
  * }
  * 
  * }}}
  * 
  * The expected output, depending on the logging configuration, would be:
  * {{{
  * [2025-01-13T12:24:23.0Z] INFO UserService - Creating user...
  * [2025-01-13T12:24:23.0Z] INFO HandleService user=n32fkj43asxa45ak - Creating handle...
  * }}}
  * 
  * This approach introduces a slight overhead, because it requires creating
  * both `LogOf` and `Log` instances on each call, but allows passing an
  * additional context to the called methods.
  * 
  * @see [[https://slf4j.org/api/org/slf4j/LoggerFactory.html LoggerFactory]]
  * for a typical underlying implementation.
  */
trait LogOf[F[_]] {

  def apply(source: String): F[Log[F]]

  def apply(source: Class[?]): F[Log[F]]
}

object LogOf {

  def apply[F[_]](implicit F: LogOf[F]): LogOf[F] = F

  def summon[F[_]](implicit F: LogOf[F]): LogOf[F] = F

  def log[F[_]: LogOf, C: ClassTag]: F[Log[F]] = apply[F].forClass[C]

  def log[F[_]: LogOf](name: String): F[Log[F]] = apply[F].apply(name)

  def apply[F[_] : Sync](factory: ILoggerFactory): LogOf[F] = new LogOf[F] {

    def apply(source: String) = {
      for {
        log <- Sync[F].delay { factory.getLogger(source) }
      } yield {
        Log[F](log)
      }
    }
    def apply(source: Class[?]) = apply(source.getName.stripSuffix("$"))
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

  @deprecated("use `slf4j` instead", "3.4.0")
  def logback[F[_] : Sync]: F[LogOf[F]] = slf4j[F]

  def empty[F[_] : Applicative]: LogOf[F] = const(Log.empty[F].pure[F])


  def console[F[_]: Monad: Console]: LogOf[F] = new LogOf[F] {

    def apply(source: String) = Log.console[F](source).pure[F]

    def apply(source: Class[?]) = apply(source.getName.stripSuffix("$"))
  }

  def const[F[_]](log: F[Log[F]]): LogOf[F] = new LogOf[F] {

    def apply(source: String) = log

    def apply(source: Class[?]) = log
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

      def apply(source: Class[?]) = {
        for {
          log <- f(self(source))
        } yield {
          log.mapK(f)
        }
      }
    }

    def forClass[C](implicit C: ClassTag[C]): F[Log[F]] = self.apply(C.runtimeClass)

    def mapLog(f: Log[F] => Log[F])(implicit F: Monad[F]): LogOf[F] = new MapLog(self, f)

    def prefixed(prefix: String)(implicit F: Monad[F]): LogOf[F] = mapLog(_.prefixed(prefix))

    def withField(key: String, value: String)(implicit F: Monad[F]): LogOf[F] = mapLog(_.withField(key, value))
  }

  private class MapLog[F[_]: Monad](logOf: LogOf[F], mapper: Log[F] => Log[F]) extends LogOf[F] {
    override def apply(source: String): F[Log[F]] = logOf.apply(source).map(mapper)
    override def apply(source: Class[?]): F[Log[F]] = logOf.apply(source).map(mapper)
  }
}
