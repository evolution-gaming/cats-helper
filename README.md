# Cats Helper [![Build Status](https://travis-ci.org/evolution-gaming/cats-helper.svg)](https://travis-ci.org/evolution-gaming/cats-helper) [![Coverage Status](https://coveralls.io/repos/evolution-gaming/cats-helper/badge.svg)](https://coveralls.io/r/evolution-gaming/cats-helper) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/69204a35e17b4e068db5861524bef5b7)](https://www.codacy.com/app/evolution-gaming/cats-helper?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=evolution-gaming/cats-helper&amp;utm_campaign=Badge_Grade) [ ![version](https://api.bintray.com/packages/evolutiongaming/maven/cats-helper/images/download.svg) ](https://bintray.com/evolutiongaming/maven/cats-helper/_latestVersion) [![License: MIT](https://img.shields.io/badge/License-MIT-yellowgreen.svg)](https://opensource.org/licenses/MIT)

## ClockHelper

```scala
import com.evolutiongaming.catshelper.ClockHelper._

val clock = Clock.const[Id](nanos = 1000, millis = 2)

clock.millis // 2
clock.nanos // 1000
clock.micros // 1
clock.instant // Instant.ofEpochMilli(2)
```

## SerialRef

Like [`Ref`](https://typelevel.org/cats-effect/concurrency/ref.html) but allows `A => F[A]` rather than `A => A`  
Ensures that updates are run serially

```scala
import com.evolutiongaming.catshelper.SerialRef

for {
  ref <- SerialRef.of[IO, Int](0)
  _   <- ref.update(a => (a + 1).pure[IO])
} yield {}
```

## LazyVal

Functional alternative to `lazy` keyword in Scala

```scala
trait LazyVal[F[_], A] {

  def get: F[A]

  def getLoaded: F[Option[A]]
}
```

## ToFuture & FromFuture

```scala
trait ToFuture[F[_]] {
  def apply[A](fa: F[A]): Future[A]
}

trait FromFuture[F[_]] {
  def apply[A](future: => Future[A]): F[A]
}
```

## ToTry & FromTry

```scala
trait ToTry[F[_]] {

  def apply[A](fa: F[A]): Try[A]
}

trait FromTry[F[_]] {

  def apply[A](fa: Try[A]): F[A]
}
```

## Log

```scala
trait Log[F[_]] {

  def debug(msg: => String): F[Unit]

  def info(msg: => String): F[Unit]

  def warn(msg: => String): F[Unit]

  def warn(msg: => String, cause: Throwable): F[Unit]

  def error(msg: => String): F[Unit]

  def error(msg: => String, cause: Throwable): F[Unit]
}
```

## Runtime

```scala
trait Runtime[F[_]] {

  def availableCores: F[Int]

  def freeMemory: F[Long]

  def totalMemory: F[Long]

  def maxMemory: F[Long]

  def gc: F[Unit]
}
```

## ThreadLocalRef

```scala
trait ThreadLocalRef[F[_], A] {

  def get: F[A]

  def set(a: A): F[Unit]

  def update(f: A => A): F[Unit]

  def modify[B](f: A => (A, B)): F[B]
}
``` 

## Setup

```scala
resolvers += Resolver.bintrayRepo("evolutiongaming", "maven")

libraryDependencies += "com.evolutiongaming" %% "cats-helper" % "0.0.4"
```
