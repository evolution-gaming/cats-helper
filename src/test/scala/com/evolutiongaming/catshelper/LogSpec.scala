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
      log = log0.mapK(FunctionK.id).prefixed(">")
      _ <- log.debug("debug")
      _ <- log.info("info")
      _ <- log.warn("warn")
      _ <- log.error("error")
      _ <- log.error("error", Error)
    } yield {}


    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Error2("> error", Error),
      Action.Error1("> error"),
      Action.Warn("> warn"),
      Action.Info("> info"),
      Action.Debug("> debug"),
      Action.OfStr("source")))
  }
}

object LogSpec {

  val logOf: LogOf[StateT] = new LogOf[StateT] {

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

  val log: Log[StateT] = new Log[StateT] {

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
        val action = Action.Warn(msg)
        (state.add(action), ())
      }
    }

    def error(msg: => String) = {
      StateT { state =>
        val action = Action.Error1(msg)
        (state.add(action), ())
      }
    }

    def error(msg: => String, cause: Throwable) = {
      StateT { state =>
        val action = Action.Error2(msg, cause)
        (state.add(action), ())
      }
    }
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
    final case class Warn(msg: String) extends Action
    final case class Error1(msg: String) extends Action
    final case class Error2(msg: String, throwable: Throwable) extends Action
  }

  case object Error extends RuntimeException with NoStackTrace
}
