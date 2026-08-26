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

Each suite says in its own Scaladoc what it measures and why.

## Baseline

One measurement of the code as it stands, so a later change has something to be compared against.
JMH scores are only comparable within one machine and one JDK, so reproduce this row before
reading anything into a difference:

| | |
| --- | --- |
| Machine | Apple M1 Pro, macOS 26.5.2 |
| JDK | OpenJDK 25.0.3 |
| Scala | 2.13.18 |
| Command | `sbt "benchmark/Jmh/run -f 1 -wi 5 -i 5 -w 2s -r 2s .*Benchmark.*"` |

Throughput in ops/s, higher is better. One operation covers 64 tasks in `SerialKeyBenchmark`,
`SerParQueueBenchmark` and `SerParQueueKeylessBenchmark`, and `tasks` tasks in `SerialBenchmark`,
so scores are only comparable within a suite.

### SerialBenchmark

| Benchmark | tasks | Score |
| --- | ---: | ---: |
| `pipelined` | 1 | 86 990 ± 12 361 |
| `pipelined` | 16 | 52 630 ± 10 047 |
| `pipelined` | 256 | 8 765 ± 163 |
| `sequential` | 1 | 93 134 ± 24 153 |
| `sequential` | 16 | 38 073 ± 5 130 |
| `sequential` | 256 | 3 100 ± 69 |

The `tasks = 1` rows carry a wide error because one operation is a single task, so the measurement
is dominated by the handoff between the calling thread and the compute pool. Read the 256 rows for
a stable figure.

### SerialKeyBenchmark

| Benchmark | implementation | keys | Score |
| --- | --- | ---: | ---: |
| `singleThread` | `partitioned` | 1 | 25 022 ± 440 |
| `singleThread` | `partitioned` | 8 | 23 354 ± 205 |
| `singleThread` | `partitioned` | 64 | 20 453 ± 1 048 |
| `singleThread` | `partitioned` | 1024 | 19 365 ± 469 |
| `singleThread` | `partitioned` | 100000 | 18 759 ± 988 |
| `singleThread` | `concurrentHashMap` | 1 | 22 915 ± 239 |
| `singleThread` | `concurrentHashMap` | 8 | 21 788 ± 788 |
| `singleThread` | `concurrentHashMap` | 64 | 18 438 ± 536 |
| `singleThread` | `concurrentHashMap` | 1024 | 18 662 ± 889 |
| `singleThread` | `concurrentHashMap` | 100000 | 18 675 ± 346 |
| `eightThreads` | `partitioned` | 1 | 53 054 ± 1 509 |
| `eightThreads` | `partitioned` | 8 | 73 448 ± 2 815 |
| `eightThreads` | `partitioned` | 64 | 61 513 ± 2 997 |
| `eightThreads` | `partitioned` | 1024 | 54 878 ± 2 855 |
| `eightThreads` | `partitioned` | 100000 | 57 109 ± 7 931 |
| `eightThreads` | `concurrentHashMap` | 1 | 23 814 ± 265 |
| `eightThreads` | `concurrentHashMap` | 8 | 62 344 ± 1 928 |
| `eightThreads` | `concurrentHashMap` | 64 | 63 100 ± 2 733 |
| `eightThreads` | `concurrentHashMap` | 1024 | 66 238 ± 5 096 |
| `eightThreads` | `concurrentHashMap` | 100000 | 72 660 ± 8 971 |

### SerParQueueBenchmark

| Benchmark | keys | Score |
| --- | ---: | ---: |
| `keyed` | 1 | 53 873 ± 4 647 |
| `keyed` | 8 | 40 759 ± 2 542 |
| `keyed` | 64 | 29 588 ± 1 185 |
| `keyed` | 256 | 22 670 ± 378 |
| `keyed` | 1024 | 21 189 ± 566 |

### SerParQueueKeylessBenchmark

| Benchmark | Score |
| --- | ---: |
| `keyless` | 53 441 ± 1 124 |

Replace this table when the baseline moves. The before and after of a single change belong in the
description of the pull request that makes it.
