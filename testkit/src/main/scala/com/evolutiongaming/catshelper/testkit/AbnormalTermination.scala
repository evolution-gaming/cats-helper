package com.evolutiongaming.catshelper.testkit

import cats.effect.testkit.TestContext

import scala.util.control.NoStackTrace

final case class AbnormalTermination(
  cause: Either[Throwable, String],
  tcState: TestContext.State,
) extends RuntimeException(s"${ cause.fold(_.toString, identity) }. $tcState", cause.left.toOption.orNull)
  with NoStackTrace
