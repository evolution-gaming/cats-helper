package com.evolution.catshelper

import cats.data.{NonEmptyList => Nel}
import com.evolution.catshelper.DataHelper._

@deprecated("use DataHelper instead", "1.2.0")
object NelHelper {

  @deprecated("use DataHelper instead", "1.2.0")
  class NelOpsNelHelper[A](val self: Nel[A]) extends AnyVal {

    def grouped(n: Int): Nel[Nel[A]] = self.groupedNel(n)
  }
}
