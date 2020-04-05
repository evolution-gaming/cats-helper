package com.evolutiongaming.catshelper

import cats.data.NonEmptyList
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AnyFreeSpec
import NonEmptyListPartitions._

class NonEmptyListPartitionOpsSpec extends AnyFreeSpec with Matchers {
  import NonEmptyListPartitionOps._

  "NonEmptyList should" - {
    "return LeftNonEmpty if first element is L" in {
      NonEmptyList.of[Any](1, 2).split({
        case i: Int    => Left(i)
        case s: String => Right(s)
      }) shouldBe AllLeft(
        NonEmptyList.of(1, 2)
      )
    }

    "return RightNonEmpty if first element is R" in {
      NonEmptyList.of[Any]("s", "r").split({
        case i: Int    => Left(i)
        case s: String => Right(s)
      }) shouldBe AllRight(
        NonEmptyList.of("s", "r")
      )
    }
    "return BothNonEmpty if first element is L" in {
      NonEmptyList.of(1, 2, "s", "r").split({
        case i: Int    => Left(i)
        case s: String => Right(s)
      }) shouldBe Both(
        NonEmptyList.of(1, 2),
        NonEmptyList.of("s", "r")
      )
    }

    "return BothNonEmpty if first element is R" in {
      NonEmptyList.of("s", "r", 1, 2).split({
        case i: Int    => Left(i)
        case s: String => Right(s)
      }) shouldBe Both(
        NonEmptyList.of(1, 2),
        NonEmptyList.of("s", "r")
      )
    }
  }
}