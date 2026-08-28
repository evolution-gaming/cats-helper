package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

/**
 * Cost of putting a task through [[Serial]] and waiting for its result.
 *
 * `pipelined` registers every task before awaiting any of them, so the queue runs a chained batch.
 * `sequential` awaits each task before registering the next, so the queue is entered from idle
 * every time. The two exercise the queue loop and the per-task wrapper in different proportions.
 *
 * To run: {{{sbt "benchmark/Jmh/run com.evolutiongaming.catshelper.SerialBenchmark"}}}
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
class SerialBenchmark {

  @Param(Array("1", "16", "256"))
  var tasks: Int = 0

  // JMH drives state through mutable fields and lifecycle hooks, so `var` is required here.
  private var runtime: IORuntime = null
  private var serial: Serial[IO] = null

  @Setup(Level.Trial)
  def setup(): Unit = {
    runtime = IORuntime.global
    serial = Serial.of[IO].unsafeRunSync()(runtime)
  }

  @Benchmark
  def pipelined(hole: Blackhole): Unit = {
    val result = List
      .fill(tasks) { serial(IO.unit) }
      .sequence
      .flatMap { _.sequence_ }
    hole.consume(result.unsafeRunSync()(runtime))
  }

  @Benchmark
  def sequential(hole: Blackhole): Unit = {
    val result = List
      .fill(tasks) { serial(IO.unit).flatten }
      .sequence_
    hole.consume(result.unsafeRunSync()(runtime))
  }
}
