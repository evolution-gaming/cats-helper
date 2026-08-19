package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.TimeUnit

/**
 * Cost of putting a task through [[SerialKey]], with the key count as the contention knob.
 *
 * [[SerialKey.of]] hash-partitions keys into one [[SerialKey]] per available core, each holding its
 * own `Ref`, to spread contention. `keys = 1` puts every thread on one key and therefore on one
 * partition, which is the worst case. Larger key counts spread the load over more partitions.
 * Compare the two columns to see what the partitioning buys.
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

  @Param(Array("1", "8", "64"))
  var keys: Int = 0

  // JMH drives state through mutable fields and lifecycle hooks, so `var` is required here.
  private var runtime: IORuntime = null
  private var serialKey: SerialKey[IO, Int] = null

  @Setup(Level.Trial)
  def setup(): Unit = {
    runtime = IORuntime.global
    serialKey = SerialKey.of[IO, Int].unsafeRunSync()(runtime)
  }

  private def enqueueAndAwait(hole: Blackhole): Unit = {
    val key = ThreadLocalRandom.current().nextInt(keys)
    val result = serialKey(key) { IO.unit }.flatten
    hole.consume(result.unsafeRunSync()(runtime))
  }

  @Benchmark
  @Threads(1)
  def singleThread(hole: Blackhole): Unit = enqueueAndAwait(hole)

  @Benchmark
  @Threads(8)
  def eightThreads(hole: Blackhole): Unit = enqueueAndAwait(hole)
}
