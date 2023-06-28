package com.evolution.catshelper

import cats.arrow.FunctionK
import cats.effect.IO
import cats.implicits._
import com.evolution.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers


class RuntimeSpec extends AsyncFunSuite with Matchers {

  private val runtime = Runtime.lift[IO].mapK(FunctionK.id)

  test("availableCores") {
    val result = for {
      cores <- runtime.availableCores
      _       = cores should be > 0
    } yield {}
    result.run()
  }

  test("freeMemory") {
    val result = for {
      cores <- runtime.freeMemory
      _       = cores should be > 0L
    } yield {}
    result.run()
  }

  test("totalMemory") {
    val result = for {
      cores <- runtime.totalMemory
      _       = cores should be > 0L
    } yield {}
    result.run()
  }

  test("maxMemory") {
    val result = for {
      cores <- runtime.maxMemory
      _       = cores should be > 0L
    } yield {}
    result.run()
  }

  test("gc") {
    runtime.gc.run()
  }
}
