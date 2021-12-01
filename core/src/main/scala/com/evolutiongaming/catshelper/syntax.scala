package com.evolutiongaming.catshelper

object syntax {
  final val timer = TimerHelper
  final val parallel = ParallelHelper
  final val clock = ClockHelper
  final val data = DataHelper
  final val contextShift = ContextShiftHelper


  object all extends CatsSyntax with ClockSyntax with DataSyntax with ContextShiftSyntax with TimerSyntax with ParallelSyntax
}
