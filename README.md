# Cats-Helper [![Build Status](https://travis-ci.org/evolution-gaming/cats-helper.svg)](https://travis-ci.org/evolution-gaming/cats-helper) [![Coverage Status](https://coveralls.io/repos/evolution-gaming/cats-helper/badge.svg)](https://coveralls.io/r/evolution-gaming/cats-helper) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/69204a35e17b4e068db5861524bef5b7)](https://www.codacy.com/app/evolution-gaming/cats-helper?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=evolution-gaming/cats-helper&amp;utm_campaign=Badge_Grade) [ ![version](https://api.bintray.com/packages/evolutiongaming/maven/cats-helper/images/download.svg) ](https://bintray.com/evolutiongaming/maven/cats-helper/_latestVersion) [![License: MIT](https://img.shields.io/badge/License-MIT-yellowgreen.svg)](https://opensource.org/licenses/MIT)

## ClockHelper

```scala
import com.evolutiongaming.catshelper.ClockHelper._

val clock = Clock.const[Id](nanos = 1000, millis = 2)

clock.millis // 2
clock.nanos // 1000
clock.micros // 1
clock.instant // Instant.ofEpochMilli(2)
```

## Setup

```scala
resolvers += Resolver.bintrayRepo("evolutiongaming", "maven")

libraryDependencies += "com.evolutiongaming" %% "cats-helper" % "0.0.1"
```
