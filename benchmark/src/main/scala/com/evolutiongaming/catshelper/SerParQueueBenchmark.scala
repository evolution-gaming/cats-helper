package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.TimeUnit

/**
 * Cost of putting a task through [[SerParQueue]], for both task kinds it accepts.
 *
 * A keyed task orders against tasks of the same key, a keyless one orders against everything, so
 * `keyless` is the barrier case and is expected to be the slower of the two.
 *
 * To run: {{{sbt "benchmark/Jmh/run com.evolutiongaming.catshelper.SerParQueueBenchmark"}}}
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
class SerParQueueBenchmark {

  @Param(Array("1", "8", "64", "256", "1024"))
  var keys: Int = 0

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
  def keyed(hole: Blackhole): Unit = {
    val key = ThreadLocalRandom.current().nextInt(keys)
    val result = queue(key.some) { IO.unit }.flatten
    hole.consume(result.unsafeRunSync()(runtime))
  }

  @Benchmark
  @Threads(8)
  def keyless(hole: Blackhole): Unit = {
    val result = queue(none[Int]) { IO.unit }.flatten
    hole.consume(result.unsafeRunSync()(runtime))
  }
}
