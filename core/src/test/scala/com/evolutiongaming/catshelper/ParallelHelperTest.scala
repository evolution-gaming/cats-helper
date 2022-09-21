package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.syntax.all._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers
import com.evolutiongaming.catshelper.ParallelHelper._
import com.evolutiongaming.catshelper.IOSuite._

class ParallelHelperTest extends AsyncFunSuite with Matchers {

  test("parFoldMapTraversable") {
    val zero = 10000
    List
      .fill(zero)(1)
      .parFoldMapTraversable { a => (a + 1).pure[IO] }
      .flatMap { a => IO { a shouldEqual zero * 2 } }
      .run()
  }

  test("parFoldMap1") {
    val zero = 10000
    List
      .fill(zero)(1)
      .parFoldMap1 { a => (a + 1).pure[IO] }
      .flatMap { a => IO { a shouldEqual zero * 2 } }
      .run()
  }
}
