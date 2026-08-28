package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

/**
 * Cost of putting a keyless task through [[SerParQueue]], which orders against every other task.
 *
 * Separate from [[SerParQueueBenchmark]] because a keyless task has no key, so a `keys` parameter
 * would only repeat the same measurement.
 *
 * One operation enqueues `tasksPerOp` tasks and awaits them all, for the reason given in
 * [[SerParQueueBenchmark]].
 *
 * To run: {{{sbt "benchmark/Jmh/run com.evolutiongaming.catshelper.SerParQueueKeylessBenchmark"}}}
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
class SerParQueueKeylessBenchmark {

  private val tasksPerOp = 64

  // JMH drives state through mutable fields and lifecycle hooks, so `var` is required here.
  private var runtime: IORuntime = null
  private var queue: SerParQueue[IO, Int] = null

  @Setup(Level.Trial)
  def setup(): Unit = {
    runtime = IORuntime.global
    queue = SerParQueue.of[IO, Int].unsafeRunSync()(runtime)
  }

  @Benchmark
  @Threads(8)
  def keyless(hole: Blackhole): Unit = {
    val result = List
      .fill(tasksPerOp) { queue(none[Int]) { IO.unit } }
      .sequence
      .flatMap { _.sequence_ }
    hole.consume(result.unsafeRunSync()(runtime))
  }
}
