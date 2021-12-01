package com.evolutiongaming.catshelper

import cats.Applicative
import cats.effect.ContextShift
import cats.implicits._

import scala.concurrent.ExecutionContext

object ContextShiftHelper extends ContextShiftSyntax

trait ContextShiftSyntax {

  import ContextShiftHelperOps._

  implicit def toContextShiftObjContextShiftHelper(self: ContextShift.type) = new ContextShiftObjContextShiftHelper(self)
}


private[catshelper] object ContextShiftHelperOps {

  implicit class ContextShiftObjContextShiftHelper(val self: ContextShift.type) extends AnyVal {

    def empty[F[_] : Applicative]: ContextShift[F] = new ContextShift[F] {

      val shift = ().pure[F]

      def evalOn[A](ec: ExecutionContext)(fa: F[A]) = fa
    }
  }
}
