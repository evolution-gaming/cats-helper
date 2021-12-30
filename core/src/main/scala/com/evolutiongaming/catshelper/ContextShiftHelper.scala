package com.evolutiongaming.catshelper

import cats.Applicative
import cats.implicits._

import scala.concurrent.ExecutionContext

object ContextShiftHelper {

  implicit class ContextShiftObjContextShiftHelper(val self: ContextShift.type) extends AnyVal {

    def empty[F[_]: Applicative]: ContextShift[F] = new ContextShift[F] {

      val shift = ().pure[F]

      def evalOn[A](ec: ExecutionContext)(fa: F[A]) = fa
    }
  }
}
