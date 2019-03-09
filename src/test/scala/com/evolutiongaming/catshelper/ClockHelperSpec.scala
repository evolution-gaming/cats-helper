package com.evolutiongaming.catshelper

import java.time.Instant

import cats.Id
import cats.effect.Clock
import com.evolutiongaming.catshelper.ClockHelper._
import org.scalatest.{FunSuite, Matchers}

class ClockHelperSpec extends FunSuite with Matchers {

  private val clock = Clock.const[Id](nanos = 1000, millis = 2)

  test("millis") {
    clock.millis shouldEqual 2
  }

  test("nanos") {
    clock.nanos shouldEqual 1000
  }

  test("micros") {
    clock.micros shouldEqual 1
  }

  test("instant") {
    clock.instant shouldEqual Instant.ofEpochMilli(2)
  }

  test("empty") {
    Clock.empty[Id].millis shouldEqual 0
    Clock.empty[Id].nanos shouldEqual 0
  }
}
