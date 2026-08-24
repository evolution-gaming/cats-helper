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
| `pipelined` | 1 | 96 508 ± 5 220 |
| `pipelined` | 16 | 54 023 ± 1 812 |
| `pipelined` | 256 | 7 458 ± 7 190 |
| `sequential` | 1 | 86 605 ± 35 975 |
| `sequential` | 16 | 37 816 ± 3 827 |
| `sequential` | 256 | 3 105 ± 715 |

### SerialKeyBenchmark

| Benchmark | implementation | keys | Score |
| --- | --- | ---: | ---: |
| `singleThread` | `partitioned` | 1 | 23 947 ± 1 022 |
| `singleThread` | `partitioned` | 8 | 22 043 ± 2 606 |
| `singleThread` | `partitioned` | 64 | 17 829 ± 4 302 |
| `singleThread` | `partitioned` | 1024 | 17 614 ± 3 646 |
| `singleThread` | `partitioned` | 100000 | 17 550 ± 2 468 |
| `singleThread` | `concurrentHashMap` | 1 | 21 794 ± 1 850 |
| `singleThread` | `concurrentHashMap` | 8 | 18 783 ± 2 611 |
| `singleThread` | `concurrentHashMap` | 64 | 17 872 ± 1 567 |
| `singleThread` | `concurrentHashMap` | 1024 | 17 043 ± 2 191 |
| `singleThread` | `concurrentHashMap` | 100000 | 15 626 ± 3 463 |
| `eightThreads` | `partitioned` | 1 | 47 040 ± 9 770 |
| `eightThreads` | `partitioned` | 8 | 68 526 ± 2 895 |
| `eightThreads` | `partitioned` | 64 | 55 896 ± 6 863 |
| `eightThreads` | `partitioned` | 1024 | 51 997 ± 7 414 |
| `eightThreads` | `partitioned` | 100000 | 52 924 ± 5 862 |
| `eightThreads` | `concurrentHashMap` | 1 | 23 518 ± 365 |
| `eightThreads` | `concurrentHashMap` | 8 | 60 812 ± 1 341 |
| `eightThreads` | `concurrentHashMap` | 64 | 59 884 ± 7 417 |
| `eightThreads` | `concurrentHashMap` | 1024 | 63 851 ± 9 909 |
| `eightThreads` | `concurrentHashMap` | 100000 | 65 669 ± 16 950 |

### SerParQueueBenchmark

| Benchmark | keys | Score |
| --- | ---: | ---: |
| `keyed` | 1 | 269 360 ± 23 980 |
| `keyed` | 8 | 273 713 ± 6 436 |
| `keyed` | 64 | 283 340 ± 16 538 |
| `keyed` | 256 | 275 698 ± 30 912 |
| `keyed` | 1024 | 269 992 ± 79 635 |
| `keyless` | 1 | 261 262 ± 8 141 |
| `keyless` | 8 | 267 356 ± 10 629 |
| `keyless` | 64 | 258 759 ± 9 000 |
| `keyless` | 256 | 264 028 ± 8 576 |
| `keyless` | 1024 | 218 094 ± 203 352 |

Replace this table when the baseline moves. The before and after of a single change belong in the
description of the pull request that makes it.
