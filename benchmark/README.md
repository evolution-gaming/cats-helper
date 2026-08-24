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

Throughput in ops/s, higher is better. One `SerialKeyBenchmark` operation covers 64 tasks, so its
scores are not comparable with the other two suites, and its rows are for
`implementation=partitioned`.

| Suite | Benchmark | keys | tasks | Score |
| --- | --- | ---: | ---: | ---: |
| `SerialBenchmark` | `pipelined` | | 1 | 100 718 ± 2 722 |
| `SerialBenchmark` | `pipelined` | | 16 | 54 267 ± 641 |
| `SerialBenchmark` | `pipelined` | | 256 | 9 564 ± 404 |
| `SerialBenchmark` | `sequential` | | 1 | 105 192 ± 1 540 |
| `SerialBenchmark` | `sequential` | | 16 | 38 812 ± 7 096 |
| `SerialBenchmark` | `sequential` | | 256 | 3 073 ± 16 |
| `SerialKeyBenchmark` | `singleThread` | 1 | | 25 270 ± 288 |
| `SerialKeyBenchmark` | `singleThread` | 8 | | 23 208 ± 626 |
| `SerialKeyBenchmark` | `singleThread` | 64 | | 18 494 ± 2 394 |
| `SerialKeyBenchmark` | `singleThread` | 256 | | 19 046 ± 549 |
| `SerialKeyBenchmark` | `singleThread` | 1024 | | 18 798 ± 427 |
| `SerialKeyBenchmark` | `eightThreads` | 1 | | 48 252 ± 815 |
| `SerialKeyBenchmark` | `eightThreads` | 8 | | 71 149 ± 3 429 |
| `SerialKeyBenchmark` | `eightThreads` | 64 | | 59 476 ± 6 671 |
| `SerialKeyBenchmark` | `eightThreads` | 256 | | 53 899 ± 1 685 |
| `SerialKeyBenchmark` | `eightThreads` | 1024 | | 52 410 ± 7 503 |
| `SerParQueueBenchmark` | `keyed` | 1 | | 257 192 ± 8 653 |
| `SerParQueueBenchmark` | `keyed` | 8 | | 270 765 ± 18 719 |
| `SerParQueueBenchmark` | `keyed` | 64 | | 273 563 ± 15 662 |
| `SerParQueueBenchmark` | `keyed` | 256 | | 273 664 ± 9 267 |
| `SerParQueueBenchmark` | `keyed` | 1024 | | 273 597 ± 16 786 |
| `SerParQueueBenchmark` | `keyless` | 1 | | 258 808 ± 6 657 |
| `SerParQueueBenchmark` | `keyless` | 8 | | 257 395 ± 11 988 |
| `SerParQueueBenchmark` | `keyless` | 64 | | 259 078 ± 8 660 |
| `SerParQueueBenchmark` | `keyless` | 256 | | 259 765 ± 10 935 |
| `SerParQueueBenchmark` | `keyless` | 1024 | | 257 541 ± 8 078 |

Replace this table when the baseline moves. The before and after of a single change belong in the
description of the pull request that makes it.
