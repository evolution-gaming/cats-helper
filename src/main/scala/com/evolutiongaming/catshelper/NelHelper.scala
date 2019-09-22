package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}

object NelHelper {

  implicit class NelOpsNelHelper[A](val self: Nel[A]) extends AnyVal {

    def grouped(n: Int): Nel[Nel[A]] = {
      if (n <= 0) Nel.of(self)
      else {
        val groups = for {
          a <- self.toList.grouped(n).toList
        } yield {
          Nel(a.head, a.tail)
        }
        Nel(groups.head, groups.tail)
      }
    }
  }
}