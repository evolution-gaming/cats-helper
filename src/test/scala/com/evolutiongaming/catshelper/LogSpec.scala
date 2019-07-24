package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import org.scalatest.{FunSuite, Matchers}

import scala.util.control.NoStackTrace

class LogSpec extends FunSuite with Matchers {

  import LogSpec._

  test("debug, info, warn, error") {

    val stateT = for {
      log0 <- logOf("source")
      log   = log0.prefixed(">").mapK(FunctionK.id)
      _    <- log.debug("debug")
      _    <- log.info("info")
      _    <- log.warn("warn")
      _    <- log.warn("warn", Error)
      _    <- log.error("error")
      _    <- log.error("error", Error)
    } yield {}


    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Error1("> error", Error),
      Action.Error0("> error"),
      Action.Warn1("> warn", Error),
      Action.Warn0("> warn"),
      Action.Info("> info"),
      Action.Debug("> debug"),
      Action.OfStr("source")))
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

      def apply(source: Class[_]) = {
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

      def debug(msg: => String) = {
        StateT { state =>
          val action = Action.Debug(msg)
          (state.add(action), ())
        }
      }

      def info(msg: => String) = {
        StateT { state =>
          val action = Action.Info(msg)
          (state.add(action), ())
        }
      }

      def warn(msg: => String) = {
        StateT { state =>
          val action = Action.Warn0(msg)
          (state.add(action), ())
        }
      }

      def warn(msg: => String, cause: Throwable) = {
        StateT { state =>
          val action = Action.Warn1(msg, cause)
          (state.add(action), ())
        }
      }

      def error(msg: => String) = {
        StateT { state =>
          val action = Action.Error0(msg)
          (state.add(action), ())
        }
      }

      def error(msg: => String, cause: Throwable) = {
        StateT { state =>
          val action = Action.Error1(msg, cause)
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
    final case class OfClass(source: Class[_]) extends Action
    final case class Debug(msg: String) extends Action
    final case class Info(msg: String) extends Action
    final case class Warn0(msg: String) extends Action
    final case class Warn1(msg: String, throwable: Throwable) extends Action
    final case class Error0(msg: String) extends Action
    final case class Error1(msg: String, throwable: Throwable) extends Action
  }

  case object Error extends RuntimeException with NoStackTrace
}
