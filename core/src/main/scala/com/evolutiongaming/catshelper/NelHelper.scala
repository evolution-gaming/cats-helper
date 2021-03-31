package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import com.evolutiongaming.catshelper.DataHelper._

@deprecated("use DataHelper instead", "1.2.0")
object NelHelper {

  @deprecated("use DataHelper instead", "1.2.0")
  class NelOpsNelHelper[A](val self: Nel[A]) extends AnyVal {

    def groupedNel(n: Int): Nel[Nel[A]] = self.groupedNel(n)
  }
}
