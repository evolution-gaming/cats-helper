package com.evolutiongaming.catshelper

import com.evolutiongaming.catshelper.syntax.LoggerInterpolatorSyntax.Interpolator
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import LogSpec._

class LoggerInterpolatorSyntaxSpec extends AnyFunSuite with Matchers {

  test("trace, debug, info, warn, error works with LoggerInterpolator syntax") {
    implicit val log0 = log

    val stateT = for {
      _ <- trace"trace"
      _ <- debug"debug"
      _ <- info"info"
      _ <- warn"warn"
      _ <- error"error"
    } yield {}


    val (state, _) = stateT.run(State(Nil))
    state shouldEqual State(List(
      Action.Error0("error"),
      Action.Warn0("warn"),
      Action.Info("info"),
      Action.Debug("debug"),
      Action.Trace("trace"),
    ))
  }
}
