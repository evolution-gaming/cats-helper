# Benchmarks

JMH benchmarks for `cats-helper`. The module is not part of the aggregate build, so `sbt test` and
CI never run it. Run them by hand:

```sh
sbt benchmark/Jmh/run                                                       # everything
sbt "benchmark/Jmh/run com.evolutiongaming.catshelper.SerialKeyBenchmark"   # one suite
sbt "benchmark/Jmh/run -prof gc SerialBenchmark.pipelined"                  # allocation too
```

`-f` forks, `-wi`/`-w` warmup iterations and duration, `-i`/`-r` measurement iterations and
duration, `-p name=value` pins a `@Param`, `-rf json -rff out.json` saves results. These override
the annotations on the class.

Each suite says in its own scaladoc what it measures and why.

## Baseline

One measurement of the code as it stands, so a later change has something to be compared against.
JMH scores are only comparable within one machine and one JDK, so reproduce this row before
reading anything into a difference:

| | |
| --- | --- |
| Commit | `c9eeb01` |
| Machine | Apple M1 Pro, macOS 26.5.2 |
| JDK | OpenJDK 25.0.3 |
| Scala | 2.13.18 |
| Command | `sbt "benchmark/Jmh/run -f 1 -wi 5 -i 5 -w 2s -r 2s .*Benchmark.*"` |

Throughput in ops/s, higher is better.

| Suite | Benchmark | keys | tasks | Score |
| --- | --- | ---: | ---: | ---: |
| `SerialBenchmark` | `pipelined` | | 1 | 100 335 ± 1 513 |
| `SerialBenchmark` | `pipelined` | | 16 | 51 002 ± 1 941 |
| `SerialBenchmark` | `pipelined` | | 256 | 9 297 ± 948 |
| `SerialBenchmark` | `sequential` | | 1 | 90 810 ± 12 626 |
| `SerialBenchmark` | `sequential` | | 16 | 37 408 ± 5 721 |
| `SerialBenchmark` | `sequential` | | 256 | 3 151 ± 56 |
| `SerialKeyBenchmark` | `singleThread` | 1 | | 100 411 ± 5 469 |
| `SerialKeyBenchmark` | `singleThread` | 8 | | 97 471 ± 13 026 |
| `SerialKeyBenchmark` | `singleThread` | 64 | | 95 699 ± 13 213 |
| `SerialKeyBenchmark` | `eightThreads` | 1 | | 261 417 ± 12 493 |
| `SerialKeyBenchmark` | `eightThreads` | 8 | | 277 859 ± 11 734 |
| `SerialKeyBenchmark` | `eightThreads` | 64 | | 273 137 ± 24 661 |
| `SerParQueueBenchmark` | `keyed` | 1 | | 261 167 ± 20 741 |
| `SerParQueueBenchmark` | `keyed` | 8 | | 272 437 ± 26 488 |
| `SerParQueueBenchmark` | `keyed` | 64 | | 274 495 ± 23 539 |
| `SerParQueueBenchmark` | `keyless` | 1 | | 260 309 ± 17 944 |
| `SerParQueueBenchmark` | `keyless` | 8 | | 255 453 ± 23 083 |
| `SerParQueueBenchmark` | `keyless` | 64 | | 258 685 ± 11 854 |

Replace this table when the baseline moves. The before and after of a single change belong in the
description of the pull request that makes it.
