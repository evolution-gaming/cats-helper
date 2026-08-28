package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.TimeUnit

/**
 * Cost of putting a keyed task through [[SerParQueue]], which orders against tasks of the same key.
 *
 * The keyless case has no key to vary, so it lives in [[SerParQueueKeylessBenchmark]] rather than
 * repeating once per `keys` value here.
 *
 * One operation enqueues `tasksPerOp` tasks and awaits them all. Enqueueing a single task per
 * operation measures the handoff between the calling thread and the compute pool rather than the
 * queue, which on this workload swamps the result.
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
  def keyed(hole: Blackhole): Unit = {
    val random = ThreadLocalRandom.current()
    val result = List
      .fill(tasksPerOp) { random.nextInt(keys) }
      .traverse { key => queue(key.some) { IO.unit } }
      .flatMap { _.sequence_ }
    hole.consume(result.unsafeRunSync()(runtime))
  }
}
