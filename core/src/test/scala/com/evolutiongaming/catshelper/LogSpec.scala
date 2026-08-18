package com.evolutiongaming.catshelper

import cats.Id
import cats.Show
import cats.arrow.FunctionK
import cats.effect.kernel.Ref
import cats.effect.std.Console
import cats.effect.{IO, SyncIO}
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.slf4j.{Logger, MDC}

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.charset.Charset
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters._
import scala.util.control.NoStackTrace

class LogSpec extends AnyFunSuite with Matchers {

  import LogSpec._

  test("trace, debug, info, warn, error") {

    val stateT = for {
      log0 <- logOf("source")
      log = log0.prefixed(">").mapK(FunctionK.id)
      _ <- log.trace("trace")
      _ <- log.debug("debug")
      _ <- log.info("info")
      _ <- log.warn("warn")
      _ <- log.warn("warn", Error)
      _ <- log.error("error")
      _ <- log.error("error", Error)
    } yield {}

    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Error1("> error", Error),
      Action.Error0("> error"),
      Action.Warn1("> warn", Error),
      Action.Warn0("> warn"),
      Action.Info("> info"),
      Action.Debug("> debug"),
      Action.Trace("> trace"),
      Action.OfStr("source"),
    ))
  }

  test("trace, debug, info, warn, error with MDC") {

    val mdc = "label" -> "value"

    val stateT = for {
      log0 <- logOf("source")
      log = log0.prefixed(">").mapK(FunctionK.id)
      _ <- log.trace("trace", Log.Mdc.Lazy(mdc))
      _ <- log.debug("debug", Log.Mdc.Lazy(mdc))
      _ <- log.info("info", Log.Mdc.Lazy(mdc))
      _ <- log.warn("warn", Log.Mdc.Lazy(mdc))
      _ <- log.warn("warn", Error, Log.Mdc.Lazy(mdc))
      _ <- log.error("error", Log.Mdc.Lazy(mdc))
      _ <- log.error("error", Error, Log.Mdc.Lazy(mdc))
    } yield {}

    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Error1("> error", Error, Log.Mdc.Lazy(mdc)),
      Action.Error0("> error", Log.Mdc.Lazy(mdc)),
      Action.Warn1("> warn", Error, Log.Mdc.Lazy(mdc)),
      Action.Warn0("> warn", Log.Mdc.Lazy(mdc)),
      Action.Info("> info", Log.Mdc.Lazy(mdc)),
      Action.Debug("> debug", Log.Mdc.Lazy(mdc)),
      Action.Trace("> trace", Log.Mdc.Lazy(mdc)),
      Action.OfStr("source"),
    ))
  }

  test("trace, debug, info, warn, error with preset MDC") {

    val mdc = Log.Mdc.Eager("label" -> "preset")

    val stateT = for {
      log0 <- logOf("source")
      log = log0.withMdc(mdc)
      _ <- log.trace("trace")
      _ <- log.debug("debug")
      _ <- log.info("info")
      _ <- log.warn("warn")
      _ <- log.warn("warn", Error)
      _ <- log.error("error")
      _ <- log.error("error", Error)
    } yield {}

    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Error1("error", Error, mdc),
      Action.Error0("error", mdc),
      Action.Warn1("warn", Error, mdc),
      Action.Warn0("warn", mdc),
      Action.Info("info", mdc),
      Action.Debug("debug", mdc),
      Action.Trace("trace", mdc),
      Action.OfStr("source"),
    ))
  }

  test("preset MDC override by in-place MDC") {

    val label = "whatever"
    val mdc0 = Log.Mdc.Eager(label -> "initial value")
    val mdc1 = Log.Mdc.Eager(label -> "overridden value")

    val stateT = for {
      log0 <- logOf("source")
      log = log0.withMdc(mdc0)
      _ <- log.warn("warn")
      _ <- log.info("info", mdc1)
      _ <- log.error("error")
    } yield {}

    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Error0("error", mdc0),
      Action.Info("info", mdc1),
      Action.Warn0("warn", mdc0),
      Action.OfStr("source"),
    ))
  }

  test("preset MDC concatenates with in-place MDC") {

    val stateT = for {
      log0 <- logOf("source")
      log = log0.withMdc(Log.Mdc.Eager("preset" -> "value"))
      _ <- log.info("info", Log.Mdc.Eager("info" -> "value"))
    } yield {}

    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Info("info", Log.Mdc.Eager("info" -> "value", "preset" -> "value")),
      Action.OfStr("source"),
    ))
  }

  test("MDC cleanup") {

    val io = for {
      logOf <- LogOf.slf4j[IO]
      log <- logOf(getClass)
      _ <- log.info("whatever", Log.Mdc.Lazy("k" -> "v"))
    } yield org.slf4j.MDC.getCopyOfContextMap

    io.unsafeRunSync() shouldEqual null
  }

  ignore("logging does not disturb the caller MDC when the logger throws") {
    val logger = Proxy
      .newProxyInstance(
        getClass.getClassLoader,
        Array(classOf[Logger]),
        new InvocationHandler {
          def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
            method.getName match {
              case "isInfoEnabled" => java.lang.Boolean.TRUE
              case "info" => throw Error
              case _ => null
            }
          }
        },
      )
      .asInstanceOf[Logger]

    def contextMap = Option(MDC.getCopyOfContextMap).fold(Map.empty[String, String])(_.asScala.toMap)

    // MDC is thread-local, so the logging must run on the thread that reads it back.
    // `SyncIO` guarantees that, `IO.unsafeRunSync` would run on a compute worker instead.
    val backup = MDC.getCopyOfContextMap
    val (before, after) = try {
      MDC.clear()
      MDC.put("caller", "value")
      val before = contextMap
      Log[SyncIO](logger).info("message", Log.Mdc.Eager("logged" -> "value")).attempt.unsafeRunSync()
      (before, contextMap)
    } finally {
      if (backup == null) MDC.clear() else MDC.setContextMap(backup)
    }

    after shouldEqual before
  }

  test("LogOf.log") {
    implicit val instance = logOf

    val (_, logByClass) = LogOf.log[StateT, AnyRef].run(State(Nil))
    val (_, logByName) = LogOf.log[StateT]("some name").run(State(Nil))

    logByClass should not be null
    logByName should not be null
  }

  test("Log.console") {
    val log = Log.console[IO]("my-test-name")
    val mdc = Log.Mdc.Lazy("answer" -> "42")
    val io = for {
      _ <- log.trace("trace msg", mdc)
      _ <- log.debug("debug msg", mdc)
      _ <- log.info("info msg", mdc)
      _ <- log.warn("warn msg", mdc)
      _ <- log.warn("warn msg", new RuntimeException("warn exception"), mdc)
      _ <- log.error("error msg", mdc)
      _ <- log.error("error msg", new RuntimeException("error exception"), mdc)
    } yield {}
    io.unsafeRunSync()
  }

  ignore("Log.console labels errors as ERROR") {
    val result = for {
      lines <- Ref[IO].of(List.empty[String])
      _ <- {
        implicit val console: Console[IO] = new Console[IO] {
          def readLineWithCharset(charset: Charset) = IO.raiseError[String](new UnsupportedOperationException)
          def print[A](
            a: A,
          )(implicit
            show: Show[A],
          ) = IO.unit
          def println[A](
            a: A,
          )(implicit
            show: Show[A],
          ) = IO.unit
          def error[A](
            a: A,
          )(implicit
            show: Show[A],
          ) = IO.unit
          def errorln[A](
            a: A,
          )(implicit
            show: Show[A],
          ) = lines.update(show.show(a) :: _)
        }

        Log.console[IO]("source").error("message")
      }
      lines <- lines.get
    } yield {
      lines should contain("ERROR\tsource: message")
    }

    result.unsafeRunSync()
  }

  ignore("withMdc does not force a lazy MDC when the level is disabled") {
    val logger = Proxy
      .newProxyInstance(
        getClass.getClassLoader,
        Array(classOf[Logger]),
        new InvocationHandler {
          def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
            val name = method.getName
            if (name.startsWith("is") && name.endsWith("Enabled")) java.lang.Boolean.FALSE else null
          }
        },
      )
      .asInstanceOf[Logger]

    val forced = new AtomicInteger(0)
    val log = Log[IO](logger).withMdc(Log.Mdc.Eager("source" -> "spec"))
    val effect = log.trace("message", Log.Mdc.Lazy("key" -> { forced.incrementAndGet(); "value" }))

    forced.get() shouldEqual 0
    effect.unsafeRunSync()
    forced.get() shouldEqual 0
  }

  ignore("lazy and eager MDC with the same content are equal") {
    val lazyMdc = Log.Mdc.Lazy("key" -> "value")
    val eagerMdc = Log.Mdc.Eager("key" -> "value")

    lazyMdc.hashCode shouldEqual eagerMdc.hashCode
    lazyMdc shouldEqual eagerMdc
    eagerMdc shouldEqual lazyMdc
  }
}

object LogSpec {

  val logOf: LogOf[StateT] = {
    val logOf = new LogOf[StateT] {

      def apply(source: String) = {
        StateT { state =>
          val action = Action.OfStr(source)
          (state.add(action), log)
        }
      }

      def apply(source: Class[?]) = {
        StateT { state =>
          val action = Action.OfClass(source)
          (state.add(action), log)
        }
      }
    }

    logOf.mapK(FunctionK.id)
  }

  val log: Log[StateT] = {
    val log = new Log[StateT] {

      def trace(msg: => String, mdc: Log.Mdc) = {
        StateT { state =>
          val action = Action.Trace(msg, mdc)
          (state.add(action), ())
        }
      }

      def debug(msg: => String, mdc: Log.Mdc) = {
        StateT { state =>
          val action = Action.Debug(msg, mdc)
          (state.add(action), ())
        }
      }

      def info(msg: => String, mdc: Log.Mdc) = {
        StateT { state =>
          val action = Action.Info(msg, mdc)
          (state.add(action), ())
        }
      }

      def warn(msg: => String, mdc: Log.Mdc) = {
        StateT { state =>
          val action = Action.Warn0(msg, mdc)
          (state.add(action), ())
        }
      }

      def warn(msg: => String, cause: Throwable, mdc: Log.Mdc) = {
        StateT { state =>
          val action = Action.Warn1(msg, cause, mdc)
          (state.add(action), ())
        }
      }

      def error(msg: => String, mdc: Log.Mdc) = {
        StateT { state =>
          val action = Action.Error0(msg, mdc)
          (state.add(action), ())
        }
      }

      def error(msg: => String, cause: Throwable, mdc: Log.Mdc) = {
        StateT { state =>
          val action = Action.Error1(msg, cause, mdc)
          (state.add(action), ())
        }
      }
    }

    log.mapK(FunctionK.id)
  }

  final case class State(actions: List[Action]) {

    def add(action: Action): State = copy(actions = action :: actions)
  }

  type StateT[A] = cats.data.StateT[Id, State, A]

  object StateT {
    def apply[A](f: State => (State, A)): StateT[A] = cats.data.StateT[Id, State, A](f)
  }

  sealed trait Action

  object Action {
    final case class OfStr(source: String) extends Action
    final case class OfClass(source: Class[?]) extends Action
    final case class Trace(msg: String, mdc: Log.Mdc = Log.Mdc.empty) extends Action
    final case class Debug(msg: String, mdc: Log.Mdc = Log.Mdc.empty) extends Action
    final case class Info(msg: String, mdc: Log.Mdc = Log.Mdc.empty) extends Action
    final case class Warn0(msg: String, mdc: Log.Mdc = Log.Mdc.empty) extends Action
    final case class Warn1(msg: String, throwable: Throwable, mdc: Log.Mdc = Log.Mdc.empty) extends Action
    final case class Error0(msg: String, mdc: Log.Mdc = Log.Mdc.empty) extends Action
    final case class Error1(msg: String, throwable: Throwable, mdc: Log.Mdc = Log.Mdc.empty) extends Action
  }

  case object Error extends RuntimeException with NoStackTrace
}
