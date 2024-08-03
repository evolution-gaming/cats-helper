package com.evolutiongaming.catshelper

import cats.effect.IO
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

class RandomIdSpec extends AsyncFunSuite with Matchers {
  test("uuid apply") {
    RandomIdOf
      .uuid[IO]
      .apply
      .map { _.value should not be empty }
      .run()
  }
}
