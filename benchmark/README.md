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
| Machine | Apple M1 Pro, macOS 26.5.2 |
| JDK | OpenJDK 25.0.3 |
| Scala | 2.13.18 |
| Command | `sbt "benchmark/Jmh/run -f 1 -wi 5 -i 5 -w 2s -r 2s .*Benchmark.*"` |

Throughput in ops/s, higher is better. One `SerialKeyBenchmark` operation covers 64 tasks, so its
scores are not comparable with the other two suites.

### SerialBenchmark

| Benchmark | tasks | Score |
| --- | ---: | ---: |
| `pipelined` | 1 | 103 279 ± 2 096 |
| `pipelined` | 16 | 56 270 ± 514 |
| `pipelined` | 256 | 9 483 ± 213 |
| `sequential` | 1 | 107 029 ± 3 921 |
| `sequential` | 16 | 39 737 ± 751 |
| `sequential` | 256 | 3 164 ± 51 |

### SerialKeyBenchmark

| Benchmark | implementation | keys | Score |
| --- | --- | ---: | ---: |
| `singleThread` | `partitioned` | 1 | 25 601 ± 126 |
| `singleThread` | `partitioned` | 8 | 22 984 ± 796 |
| `singleThread` | `partitioned` | 64 | 20 294 ± 1 120 |
| `singleThread` | `partitioned` | 1024 | 18 957 ± 816 |
| `singleThread` | `partitioned` | 100000 | 19 340 ± 1 079 |
| `singleThread` | `concurrentHashMap` | 1 | 22 591 ± 545 |
| `singleThread` | `concurrentHashMap` | 8 | 20 846 ± 788 |
| `singleThread` | `concurrentHashMap` | 64 | 18 714 ± 449 |
| `singleThread` | `concurrentHashMap` | 1024 | 18 171 ± 1 512 |
| `singleThread` | `concurrentHashMap` | 100000 | 18 546 ± 800 |
| `eightThreads` | `partitioned` | 1 | 52 111 ± 847 |
| `eightThreads` | `partitioned` | 8 | 71 944 ± 2 663 |
| `eightThreads` | `partitioned` | 64 | 58 871 ± 3 718 |
| `eightThreads` | `partitioned` | 1024 | 53 437 ± 5 839 |
| `eightThreads` | `partitioned` | 100000 | 57 099 ± 5 584 |
| `eightThreads` | `concurrentHashMap` | 1 | 23 805 ± 707 |
| `eightThreads` | `concurrentHashMap` | 8 | 61 645 ± 1 607 |
| `eightThreads` | `concurrentHashMap` | 64 | 62 234 ± 3 601 |
| `eightThreads` | `concurrentHashMap` | 1024 | 65 154 ± 10 002 |
| `eightThreads` | `concurrentHashMap` | 100000 | 72 369 ± 7 472 |

### SerParQueueBenchmark

| Benchmark | keys | Score |
| --- | ---: | ---: |
| `keyed` | 1 | 262 039 ± 10 864 |
| `keyed` | 8 | 278 905 ± 18 188 |
| `keyed` | 64 | 276 485 ± 11 631 |
| `keyed` | 256 | 275 357 ± 4 119 |
| `keyed` | 1024 | 278 134 ± 7 996 |
| `keyless` | 1 | 264 736 ± 14 070 |
| `keyless` | 8 | 260 776 ± 4 472 |
| `keyless` | 64 | 263 170 ± 3 920 |
| `keyless` | 256 | 262 007 ± 7 791 |
| `keyless` | 1024 | 263 735 ± 5 393 |

Replace this table when the baseline moves. The before and after of a single change belong in the
description of the pull request that makes it.
