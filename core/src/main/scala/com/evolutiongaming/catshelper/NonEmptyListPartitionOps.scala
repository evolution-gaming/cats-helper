package com.evolutiongaming.catshelper

import cats.data.NonEmptyList
import com.evolutiongaming.catshelper.NonEmptyListPartitions.{AllLeft, AllRight}

object NonEmptyListPartitionOps {

  implicit class NonEmptyListPartitionOpsC[T](nel: NonEmptyList[T]) {
    def split[L, R](classifier: T => Either[L, R]): NonEmptyListPartitions[L, R] = {
      val value: NonEmptyListPartitions[L, R] = classifier(nel.head).fold(AllLeft.one, AllRight.one)

      nel.tail.foldLeft(value) { case (acc, nextElement) =>
        classifier(nextElement).fold(
          acc.prependLeft,
          acc.prependRight
        )
      }.reverse
    }
  }
}

sealed trait NonEmptyListPartitions[L, R] {
  def prependLeft(l: L): NonEmptyListPartitions[L, R]
  def prependRight(r: R): NonEmptyListPartitions[L, R]
  def reverse: NonEmptyListPartitions[L, R]
}

object NonEmptyListPartitions {

  case class AllLeft[L, R](left: NonEmptyList[L]) extends NonEmptyListPartitions[L, R] {
    def prependLeft(l: L): NonEmptyListPartitions[L, R] = AllLeft(l :: left)
    def prependRight(r: R): NonEmptyListPartitions[L, R] = Both(left, NonEmptyList.one(r))
    def reverse: AllLeft[L, R] = AllLeft(left.reverse)
  }

  object AllLeft {
    def one[L, R](head: L): NonEmptyListPartitions[L, R] = AllLeft(NonEmptyList.one(head))
  }

  case class AllRight[L, R](right: NonEmptyList[R]) extends NonEmptyListPartitions[L, R] {
    def prependLeft(l: L): NonEmptyListPartitions[L, R] = Both(NonEmptyList.one(l), right)
    def prependRight(r: R): NonEmptyListPartitions[L, R] = AllRight(r :: right)
    def reverse: AllRight[L, R] = AllRight(right.reverse)
  }

  object AllRight {
    def one[L, R](head: R): NonEmptyListPartitions[L, R] = AllRight(NonEmptyList.one(head))
  }

  case class Both[L, R](left: NonEmptyList[L], right: NonEmptyList[R]) extends NonEmptyListPartitions[L, R] {
    def prependLeft(l: L): NonEmptyListPartitions[L, R] = copy(left = l :: left)
    def prependRight(r: R): NonEmptyListPartitions[L, R] = copy(right = r :: right)
    def reverse: Both[L, R] = Both(left.reverse, right.reverse)
  }
}
