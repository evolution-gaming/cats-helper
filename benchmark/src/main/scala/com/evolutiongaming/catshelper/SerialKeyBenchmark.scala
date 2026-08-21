package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.TimeUnit

/**
 * Cost of putting a task through [[SerialKey]], across backing stores and contention levels.
 *
 * `keys` is the contention knob. `keys = 1` puts every thread on one key, which is the worst case,
 * larger counts spread the load.
 *
 * One operation enqueues `tasksPerOp` tasks and awaits them all. Enqueueing a single task per
 * operation measures the handoff between the calling thread and the compute pool rather than the
 * queue, which on this workload swamps the result with noise.
 *
 * To run: {{{sbt "benchmark/Jmh/run com.evolutiongaming.catshelper.SerialKeyBenchmark"}}}
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
class SerialKeyBenchmark {

  @Param(Array("1", "8", "64", "256", "1024"))
  var keys: Int = 0

  private val tasksPerOp = 64

  // JMH drives state through mutable fields and lifecycle hooks, so `var` is required here.
  private var runtime: IORuntime = null
  private var serialKey: SerialKey[IO, Int] = null

  @Setup(Level.Trial)
  def setup(): Unit = {
    runtime = IORuntime.global
    serialKey = SerialKey.of[IO, Int].unsafeRunSync()(runtime)
  }

  private def enqueueAndAwait(hole: Blackhole): Unit = {
    val random = ThreadLocalRandom.current()
    val result = List
      .fill(tasksPerOp) { random.nextInt(keys) }
      .traverse { key => serialKey(key) { IO.unit } }
      .flatMap { _.sequence_ }
    hole.consume(result.unsafeRunSync()(runtime))
  }

  @Benchmark
  @Threads(1)
  def singleThread(hole: Blackhole): Unit = enqueueAndAwait(hole)

  @Benchmark
  @Threads(8)
  def eightThreads(hole: Blackhole): Unit = enqueueAndAwait(hole)
}
