package com.evolutiongaming.catshelper

import cats.Monad
import cats.data.NonEmptyMap
import cats.effect.Sync
import cats.effect.std.Console
import cats.syntax.all.*
import cats.{Applicative, Semigroup, ~>}
import org.slf4j.{Logger, MDC}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

/**
 * Named/prefixed logger instance.
 *
 * Use [[LogOf]] to create the new instances of the class.
 *
 * Avoid passing `Log` instances implicitly. Passing implicit instances around could "leak" a logger
 * into an unrelated code, and create a confusion with wrongly attributed log messages.
 *
 * @see
 *   [[LogOf]] for usage examples.
 * @see
 *   [[https://slf4j.org/api/org/slf4j/Logger.html Logger]] for a typical underlying implementation.
 */
trait Log[F[_]] {

  @inline def trace(msg: => String): F[Unit] = trace(msg, mdc = Log.Mdc.empty)

  @inline def debug(msg: => String): F[Unit] = debug(msg, mdc = Log.Mdc.empty)

  @inline def info(msg: => String): F[Unit] = info(msg, mdc = Log.Mdc.empty)

  @inline def warn(msg: => String): F[Unit] = warn(msg, mdc = Log.Mdc.empty)

  @inline def warn(msg: => String, cause: Throwable): F[Unit] = warn(msg, cause, mdc = Log.Mdc.empty)

  @inline def error(msg: => String): F[Unit] = error(msg, mdc = Log.Mdc.empty)

  @inline def error(msg: => String, cause: Throwable): F[Unit] = error(msg, cause, mdc = Log.Mdc.empty)

  def trace(msg: => String, mdc: Log.Mdc): F[Unit]

  def debug(msg: => String, mdc: Log.Mdc): F[Unit]

  def info(msg: => String, mdc: Log.Mdc): F[Unit]

  def warn(msg: => String, mdc: Log.Mdc): F[Unit]

  def warn(msg: => String, cause: Throwable, mdc: Log.Mdc): F[Unit]

  def error(msg: => String, mdc: Log.Mdc): F[Unit]

  def error(msg: => String, cause: Throwable, mdc: Log.Mdc): F[Unit]
}

object Log {

  sealed trait Mdc
  object Mdc {

    /**
     * Two `Mdc` values are equal when they carry the same records, no matter whether they are held
     * eagerly or behind a thunk. Comparing forces a [[LazyContext]].
     */
    private def equalsOf(self: Mdc, obj: Any): Boolean = obj match {
      case that: Mdc => contextOf(self) == contextOf(that)
      case _ => false
    }

    private def hashCodeOf(self: Mdc): Int = contextOf(self).hashCode()

    private object Empty extends Mdc {
      override def toString: String = "MDC()"

      override def hashCode(): Int = hashCodeOf(this)

      override def equals(obj: Any): Boolean = equalsOf(this, obj)
    }
    private final case class EagerContext(values: NonEmptyMap[String, String]) extends Mdc {
      override def toString: String = s"MDC(${ values.toSortedMap.mkString(", ") })"

      override def hashCode(): Int = hashCodeOf(this)

      override def equals(obj: Any): Boolean = equalsOf(this, obj)
    }
    private final class LazyContext(val getMdc: () => Mdc) extends Mdc {

      override def toString: String = getMdc().toString

      override def hashCode(): Int = hashCodeOf(this)

      override def equals(obj: Any): Boolean = equalsOf(this, obj)
    }
    private object LazyContext {
      def apply(mdc: => Mdc): LazyContext = new LazyContext(() => mdc)
    }

    /**
     * Defers building an `Mdc` until the records are actually needed.
     */
    private[Log] def defer(mdc: => Mdc): Mdc = LazyContext(mdc)

    val empty: Mdc = Empty

    type Record = (String, String)

    @deprecated("Use Mdc.Lazy.apply or Mdc.Eager.apply", "3.9.0")
    def apply(head: Record, tail: Record*): Mdc = Eager(head, tail*)

    @deprecated("Use Mdc.Lazy.fromSeq or Mdc.Eager.fromSeq", "3.9.0")
    def fromSeq(seq: Seq[Record]): Mdc = Eager.fromSeq(seq)

    @deprecated("Use Mdc.Lazy.fromMap or Mdc.Eager.fromMap", "3.9.0")
    def fromMap(map: Map[String, String]): Mdc = Eager.fromMap(map)

    object Eager {
      def apply(head: Record, tail: Record*): Mdc = EagerContext(NonEmptyMap.of(head, tail: _*))

      def fromSeq(seq: Seq[Record]): Mdc = NonEmptyMap.fromMap(SortedMap(seq: _*)).fold(empty) { nem =>
        EagerContext(nem)
      }

      def fromMap(map: Map[String, String]): Mdc = fromSeq(map.toSeq)
    }

    object Lazy {
      def apply(v1: => Record): Mdc = LazyContext(Eager(v1))
      def apply(v1: => Record, v2: => Record): Mdc = LazyContext(Eager(v1, v2))
      def apply(v1: => Record, v2: => Record, v3: => Record): Mdc = LazyContext(Eager(v1, v2, v3))
      def apply(
        v1: => Record,
        v2: => Record,
        v3: => Record,
        v4: => Record,
      ): Mdc = LazyContext(Eager(v1, v2, v3, v4))
      def apply(
        v1: => Record,
        v2: => Record,
        v3: => Record,
        v4: => Record,
        v5: => Record,
      ): Mdc =
        LazyContext(Eager(v1, v2, v3, v4, v5))
      def apply(
        v1: => Record,
        v2: => Record,
        v3: => Record,
        v4: => Record,
        v5: => Record,
        v6: => Record,
      ): Mdc =
        LazyContext(Eager(v1, v2, v3, v4, v5, v6))
      def apply(
        v1: => Record,
        v2: => Record,
        v3: => Record,
        v4: => Record,
        v5: => Record,
        v6: => Record,
        v7: => Record,
      ): Mdc =
        LazyContext(Eager(v1, v2, v3, v4, v5, v6, v7))
      def apply(
        v1: => Record,
        v2: => Record,
        v3: => Record,
        v4: => Record,
        v5: => Record,
        v6: => Record,
        v7: => Record,
        v8: => Record,
      ): Mdc =
        LazyContext(Eager(v1, v2, v3, v4, v5, v6, v7, v8))
      def apply(
        v1: => Record,
        v2: => Record,
        v3: => Record,
        v4: => Record,
        v5: => Record,
        v6: => Record,
        v7: => Record,
        v8: => Record,
        v9: => Record,
      ): Mdc =
        LazyContext(Eager(v1, v2, v3, v4, v5, v6, v7, v8, v9))
      def apply(
        v1: => Record,
        v2: => Record,
        v3: => Record,
        v4: => Record,
        v5: => Record,
        v6: => Record,
        v7: => Record,
        v8: => Record,
        v9: => Record,
        v10: => Record,
      ): Mdc =
        LazyContext(Eager(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10))

      def fromSeq(seq: => Seq[Record]): Mdc = LazyContext(Eager.fromSeq(seq))

      def fromMap(map: => Map[String, String]): Mdc = LazyContext(Eager.fromMap(map))
    }

    implicit final val mdcSemigroup: Semigroup[Mdc] = {
      @tailrec def joinContexts(c1: Mdc, c2: Mdc): Mdc = (c1, c2) match {
        case (left: LazyContext, right) => joinContexts(left.getMdc(), right)
        case (left, right: LazyContext) => joinContexts(left, right.getMdc())
        case (EagerContext(v1), EagerContext(v2)) => EagerContext(v1 ++ v2)
        case (left: EagerContext, _) => left
        case (_, right) => right
      }

      Semigroup.instance(joinContexts)
    }

    /**
     * Matches on subtypes rather than on `Empty` itself: a stable identifier pattern would call
     * `Empty.equals`, which resolves back through here.
     */
    @tailrec private def contextOf(mdc: Mdc): Option[NonEmptyMap[String, String]] = mdc match {
      case eagerContext: EagerContext => Some(eagerContext.values)
      case lazyContext: LazyContext => contextOf(lazyContext.getMdc())
      case _ => None
    }

    implicit final class MdcOps(val mdc: Mdc) extends AnyVal {

      def context: Option[NonEmptyMap[String, String]] = contextOf(mdc)
    }
  }

  def apply[F[_]](
    implicit
    F: Log[F],
  ): Log[F] = F

  def summon[F[_]](
    implicit
    F: Log[F],
  ): Log[F] = F

  def apply[F[_]: Sync](logger: Logger): Log[F] = new Log[F] {

    def withMDC(mdc: Log.Mdc)(log: => Unit): Unit = {
      import Mdc.MdcOps
      mdc.context match {
        case None => log
        case Some(mdc) =>
          val backup = MDC.getCopyOfContextMap
          try {
            MDC.clear()
            mdc.toSortedMap foreach { case (k, v) => MDC.put(k, v) }
            log
          } finally {
            if (backup == null) MDC.clear() else MDC.setContextMap(backup)
          }
      }
    }

    def trace(msg: => String, mdc: Log.Mdc) = {
      Sync[F].delay {
        if (logger.isTraceEnabled) withMDC(mdc) { logger.trace(msg) }
      }
    }

    def debug(msg: => String, mdc: Log.Mdc) = {
      Sync[F].delay {
        if (logger.isDebugEnabled) withMDC(mdc) { logger.debug(msg) }
      }
    }

    def info(msg: => String, mdc: Log.Mdc) = {
      Sync[F].delay {
        if (logger.isInfoEnabled) withMDC(mdc) { logger.info(msg) }
      }
    }

    def warn(msg: => String, mdc: Log.Mdc) = {
      Sync[F].delay {
        if (logger.isWarnEnabled) withMDC(mdc) { logger.warn(msg) }
      }
    }

    def warn(msg: => String, cause: Throwable, mdc: Log.Mdc) = {
      Sync[F].delay {
        if (logger.isWarnEnabled) withMDC(mdc) { logger.warn(msg, cause) }
      }
    }

    def error(msg: => String, mdc: Log.Mdc) = {
      Sync[F].delay {
        if (logger.isErrorEnabled) withMDC(mdc) { logger.error(msg) }
      }
    }

    def error(msg: => String, cause: Throwable, mdc: Log.Mdc) = {
      Sync[F].delay {
        if (logger.isErrorEnabled) withMDC(mdc) { logger.error(msg, cause) }
      }
    }
  }

  def const[F[_]](unit: F[Unit]): Log[F] = new Log[F] {

    def trace(msg: => String, mdc: Log.Mdc) = unit

    def debug(msg: => String, mdc: Log.Mdc) = unit

    def info(msg: => String, mdc: Log.Mdc) = unit

    def warn(msg: => String, mdc: Log.Mdc) = unit

    def warn(msg: => String, cause: Throwable, mdc: Log.Mdc) = unit

    def error(msg: => String, mdc: Log.Mdc) = unit

    def error(msg: => String, cause: Throwable, mdc: Log.Mdc) = unit
  }

  def empty[F[_]: Applicative]: Log[F] = const(Applicative[F].unit)

  @deprecated("use `console(<name>)` instead", "3.10.5")
  def console[F[_]: Monad: Console]: Log[F] = console("")

  def console[F[_]: Monad: Console](name: String): Log[F] = new Log[F] {

    val C = Console[F]

    def log(level: String, msg: => String, mdc: Log.Mdc): String = {
      val mdcStr = mdc.context match {
        case None => ""
        case Some(mdc) => mdc.toSortedMap.map { case (k, v) => s"$k=$v" }.mkString(" ", ", ", "")
      }
      s"$level\t$name$mdcStr: $msg"
    }

    override def trace(msg: => String, mdc: Mdc): F[Unit] = C.println(log("TRACE", msg, mdc))

    override def debug(msg: => String, mdc: Mdc): F[Unit] = C.println(log("DEBUG", msg, mdc))

    override def info(msg: => String, mdc: Mdc): F[Unit] = C.println(log("INFO", msg, mdc))

    override def warn(msg: => String, mdc: Mdc): F[Unit] = C.println(log("WARN", msg, mdc))

    override def warn(msg: => String, cause: Throwable, mdc: Mdc): F[Unit] = C.println(log("WARN", msg, mdc)) >>
      C.printStackTrace(cause)

    override def error(msg: => String, mdc: Mdc): F[Unit] = C.errorln(log("ERROR", msg, mdc))

    override def error(msg: => String, cause: Throwable, mdc: Mdc): F[Unit] = C.errorln(log("ERROR", msg, mdc)) >>
      C.printStackTrace(cause)

  }

  implicit class LogOps[F[_]](val self: Log[F]) extends AnyVal {

    def mapK[G[_]](f: F ~> G): Log[G] = new Log[G] {

      def trace(msg: => String, mdc: Log.Mdc) = f(self.trace(msg, mdc))

      def debug(msg: => String, mdc: Log.Mdc) = f(self.debug(msg, mdc))

      def info(msg: => String, mdc: Log.Mdc) = f(self.info(msg, mdc))

      def warn(msg: => String, mdc: Log.Mdc) = f(self.warn(msg, mdc))

      def warn(msg: => String, cause: Throwable, mdc: Log.Mdc) = f(self.warn(msg, cause, mdc))

      def error(msg: => String, mdc: Log.Mdc) = f(self.error(msg, mdc))

      def error(msg: => String, cause: Throwable, mdc: Log.Mdc) = f(self.error(msg, cause, mdc))
    }

    def mapMsg(f: String => String): Log[F] = new Log[F] {

      def trace(msg: => String, mdc: Log.Mdc) = self.trace(f(msg), mdc)

      def debug(msg: => String, mdc: Log.Mdc) = self.debug(f(msg), mdc)

      def info(msg: => String, mdc: Log.Mdc) = self.info(f(msg), mdc)

      def warn(msg: => String, mdc: Log.Mdc) = self.warn(f(msg), mdc)

      def warn(msg: => String, cause: Throwable, mdc: Log.Mdc) = self.warn(f(msg), cause, mdc)

      def error(msg: => String, mdc: Log.Mdc) = self.error(f(msg), mdc)

      def error(msg: => String, cause: Throwable, mdc: Log.Mdc) = self.error(f(msg), cause, mdc)
    }

    def prefixed(prefix: String): Log[F] = mapMsg(msg => s"$prefix $msg")

    def withField(key: String, value: String): Log[F] = prefixed(s"$key=$value")

    /**
     * `f` is applied behind [[Mdc.defer]] so that a [[Mdc.Lazy]] passed by the caller stays
     * unforced until the underlying logger decides the record is actually going to be written.
     */
    def mapMdc(f: Log.Mdc => Log.Mdc): Log[F] = new Log[F] {

      def trace(msg: => String, mdc: Mdc): F[Unit] = self.trace(msg, Mdc.defer(f(mdc)))

      def debug(msg: => String, mdc: Mdc): F[Unit] = self.debug(msg, Mdc.defer(f(mdc)))

      def info(msg: => String, mdc: Mdc): F[Unit] = self.info(msg, Mdc.defer(f(mdc)))

      def warn(msg: => String, mdc: Mdc): F[Unit] = self.warn(msg, Mdc.defer(f(mdc)))

      def warn(msg: => String, cause: Throwable, mdc: Mdc): F[Unit] = self.warn(msg, cause, Mdc.defer(f(mdc)))

      def error(msg: => String, mdc: Mdc): F[Unit] = self.error(msg, Mdc.defer(f(mdc)))

      def error(msg: => String, cause: Throwable, mdc: Mdc): F[Unit] = self.error(msg, cause, Mdc.defer(f(mdc)))
    }

    def withMdc(mdc: Log.Mdc): Log[F] = mapMdc(mdc1 => mdc |+| mdc1)
  }
}
