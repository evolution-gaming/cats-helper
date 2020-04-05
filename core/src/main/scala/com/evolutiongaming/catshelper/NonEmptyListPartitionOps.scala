package com.evolutiongaming.catshelper

import cats.data.NonEmptyList
import com.evolutiongaming.catshelper.NonEmptyListPartitions.{AllLeft, AllRight}

object NonEmptyListPartitionOps {

  implicit class NonEmptyListPartitionOpsC[T](nel: NonEmptyList[T]) {
    def split[L, R](classifier: T => Either[L, R]): NonEmptyListPartitions[L, R] = {
      val value: NonEmptyListPartitions[ L, R] = classifier(nel.head).fold(AllLeft.one, AllRight.one)

      nel.tail.foldLeft(value) { case (acc, nextElement) =>
        classifier(nextElement).fold(
          acc.appendLeft,
          acc.appendRight
        )
      }
    }
  }
}

sealed trait NonEmptyListPartitions[L, R] {
  def appendLeft(l: L): NonEmptyListPartitions[L, R]
  def appendRight(r: R): NonEmptyListPartitions[L, R]
}

object NonEmptyListPartitions {

  case class AllLeft[L, R](left: NonEmptyList[L]) extends NonEmptyListPartitions[L, R] {
    def appendLeft(l: L): NonEmptyListPartitions[L, R] = AllLeft(left :+ l)
    def appendRight(r: R): NonEmptyListPartitions[L, R] = Both(left, NonEmptyList.one(r))
  }

  object AllLeft {
    def one[L, R](head: L): NonEmptyListPartitions[L, R] = AllLeft(NonEmptyList.one(head))
  }

  case class AllRight[L, R](right: NonEmptyList[R]) extends NonEmptyListPartitions[L, R] {
    def appendLeft(l: L): NonEmptyListPartitions[L, R] = Both(NonEmptyList.one(l), right)
    def appendRight(r: R): NonEmptyListPartitions[L, R] = AllRight(right :+ r)
  }

  object AllRight {
    def one[L, R](head: R): NonEmptyListPartitions[L, R] = AllRight(NonEmptyList.one(head))
  }

  case class Both[L, R](left: NonEmptyList[L], right: NonEmptyList[R]) extends NonEmptyListPartitions[L, R] {
    def appendLeft(l: L): NonEmptyListPartitions[L, R] = copy(left = left :+ l)
    def appendRight(r: R): NonEmptyListPartitions[L, R] = copy(right = right :+ r)
  }
}
