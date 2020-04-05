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

  "NonEmptyListPartitions.reverse should" - {
    "return Both when accepting Both" in {
      Both(
        NonEmptyList.of(1, 2),
        NonEmptyList.of("hi", "bye")
      ).reverse shouldBe Both(
        NonEmptyList.of(2, 1),
        NonEmptyList.of("bye", "hi")
      )
    }

    "return AllRight when accepting AllRight" in {
      AllRight(
        NonEmptyList.of(1, 2),
      ).reverse shouldBe AllRight(
        NonEmptyList.of(2, 1)
      )
    }

    "return AllLeft when accepting AllLeft" in {
      AllLeft(
        NonEmptyList.of(1, 2),
      ).reverse shouldBe AllLeft(
        NonEmptyList.of(2, 1)
      )
    }
  }

  "NonEmptyListPartitions.prependLeft should" - {
    "return Both when accepting Both" in {
      Both(
        NonEmptyList.of(1, 2),
        NonEmptyList.of("hi", "bye")
      ).prependLeft(0) shouldBe Both(
        NonEmptyList.of(0, 1, 2),
        NonEmptyList.of("hi", "bye")
      )
    }

    "return Both when accepting AllRight" in {
      AllRight(
        NonEmptyList.of(1, 2),
      ).prependLeft("hi") shouldBe Both(
        NonEmptyList.one("hi"),
        NonEmptyList.of(1, 2)
      )
    }

    "return AllLeft when accepting AllLeft" in {
      AllLeft(
        NonEmptyList.of(1, 2),
      ).prependLeft(0) shouldBe AllLeft(
        NonEmptyList.of(0, 1, 2)
      )
    }
  }

  "NonEmptyListPartitions.prependRight should" - {
    "return Both when accepting Both" in {
      Both(
        NonEmptyList.of(1, 2),
        NonEmptyList.of("hi", "bye")
      ).prependRight("emm") shouldBe Both(
        NonEmptyList.of(1, 2),
        NonEmptyList.of("emm", "hi", "bye")
      )
    }

    "return AllRight when accepting AllRight" in {
      AllRight(
        NonEmptyList.of(1, 2),
      ).prependRight(0) shouldBe AllRight(
        NonEmptyList.of(0,1, 2)
      )
    }

    "return Both when accepting AllLeft" in {
      AllLeft(
        NonEmptyList.of(1, 2),
      ).prependRight("hi") shouldBe Both(
        NonEmptyList.of( 1, 2),
        NonEmptyList.one("hi")
      )
    }
  }

}