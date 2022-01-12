package com.evolutiongaming.catshelper

import cats.Parallel
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.effect.{Clock, Concurrent, IO, Sync, Temporal}
import cats.syntax.all._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import cats.effect.kernel.Async

class SerParQueueTest extends AsyncFunSuite with Matchers {
  import SerParQueueTest._

  private val error: Throwable = new RuntimeException with NoStackTrace

  test("run in parallel for different keys & serially for same") {
    val result = for {
      q  <- Queue.of[IO, String, Int]
      da <- Deferred[IO, Int]
      _  <- q.start("a".some) { da.get }
      a  <- q("a".some) { 1.pure[IO] }

      db <- Deferred[IO, Int]
      _  <- q.start("b".some) { db.get }
      b  <- q("b".some) { 1.pure[IO] }
      _  <- db.complete(0)
      b  <- b
      _  <- IO { b shouldEqual 1 }

      _ <- da.complete(0)
      a <- a
      _ <- IO { a shouldEqual 1 }

      rs <- q.records
      _ <- IO {
        rs shouldEqual List(Record.par(("a", List(0, 1)), ("b", List(0, 1))))
      }
    } yield {}
    result.run()
  }

  test("run in parallel keyfull in between of keyless") {
    val result = for {
      q   <- Queue.of[IO, String, Int]
      da  <- Deferred[IO, Int]
      db0 <- Deferred[IO, Unit]
      db1 <- Deferred[IO, Int]
      dc0 <- Deferred[IO, Unit]
      dc1 <- Deferred[IO, Int]

      _ <- q.start(none) { da.get }
      b <- q("b".some) { db0.complete(()) *> db1.get }
      c <- q("c".some) { dc0.complete(()) *> dc1.get }
      a <- q(none) { 1.pure[IO] }

      _ <- da.complete(0)
      _ <- db0.get
      _ <- dc0.get
      _ <- db1.complete(0)
      _ <- dc1.complete(0)

      a <- a
      _ <- IO { a shouldEqual 1 }

      b <- b
      _ <- IO { b shouldEqual 0 }

      c <- c
      _ <- IO { c shouldEqual 0 }

      rs <- q.records
      _ <- IO {
        rs shouldEqual List(Record.ser(0), Record.par(("b", List(0)), ("c", List(0))), Record.ser(1))
      }
    } yield {}
    result.run()
  }

  test("run mixed") {
    val result = for {
      q  <- Queue.of[IO, String, Int]
      d0 <- Deferred[IO, Int]
      _  <- q("a".some) { d0.get }
      _  <- q("b".some) { 0.pure[IO] }
      _  <- q("b".some) { 1.pure[IO] }
      _  <- q("c".some) { 0.pure[IO] }
      _  <- q("c".some) { 1.pure[IO] }
      _  <- q("a".some) { 1.pure[IO] }
      _  <- q(none) { 0.pure[IO] }
      _  <- q(none) { 1.pure[IO] }
      _  <- q("b".some) { 2.pure[IO] }
      b  <- q("b".some) { 3.pure[IO] }
      _  <- q("c".some) { 2.pure[IO] }
      c  <- q("c".some) { 3.pure[IO] }

      _  <- d0.complete(0)
      _  <- b
      _  <- c
      rs <- q.records
      _ <- IO {
        rs shouldEqual List(
          Record.par(("a", List(0, 1)), ("b", List(0, 1)), ("c", List(0, 1))),
          Record.ser(0),
          Record.ser(1),
          Record.par(("b", List(2, 3)), ("c", List(2, 3)))
        )
      }
    } yield {}
    result.run()
  }

  test("enqueue is not async") {
    val threadId = IO { Thread.currentThread().getId }
    val result = for {
      q <- Queue.of[IO, String, Int]
      a <- threadId
      _ <- q("a".some) { 1.pure[IO] }
      b <- threadId
      _ <- IO { a shouldEqual b }
    } yield {}
    result.run()
  }

  test("run many") {
    val tasks = 1000
    val keys  = 10

    val duration = {
      val ms = Clock[IO].monotonic.map(_.toMillis)
      ms.map { a => ms.map { b => (b - a).millis } }
    }

    val result = for {
      logOf <- LogOf.slf4j[IO]
      log   <- logOf(SerParQueueTest.getClass)
      q     <- SerParQueue.of[IO, Int]
      d     <- duration
      a <- 0
        .iterateForeverM { a =>
          for {
            _ <- q(none) { a.pure[IO] }
            _ <- Temporal[IO].sleep(100.millis)
          } yield a + 1
        }
        .background
        .use { _ =>
          (1 to keys)
            .toList
            .parTraverse { key =>
              (0, 0.pure[IO]).tailRecM {
                case (n, a) =>
                  if (n > tasks) {
                    a.asRight[(Int, IO[Int])].pure[IO]
                  } else {
                    for {
                      a <- q(key.some) { n.pure[IO] }
                    } yield {
                      (n + 1, a).asLeft[IO[Int]]
                    }
                  }
              }.flatten
            }
        }
      d <- d
      _ <- IO { log.info(s"took ${d.toMillis}ms for $keys parallel streams with $tasks each") }
      _ <- IO { a.distinct shouldEqual List(tasks) }
    } yield {}
    result.run(1.minute)
  }

  for {
    key <- List(none[Int], 0.some)
  } yield {

    test(s"$key: run") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key, "a")

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key, "a")) }
      } yield {}
      result.run()
    }

    test(s"$key: run, fail") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key, "a")

        a <- q(key) { error.raiseError[IO, String] }
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key, "a")) }
      } yield {}
      result.run()
    }

    test(s"$key: start, add, fail, run") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, Either[Throwable, String]]
        a <- q(key) { d.get.rethrow }

        b <- q(key) { "b".pure[IO] }

        _ <- d.complete(error.asLeft)
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        b <- b
        _ <- IO { b shouldEqual "b" }

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key, "b")) }
      } yield {}
      result.run()
    }

    test(s"$key: fail, run") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        a <- q(key) { error.raiseError[IO, String] }
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        _ <- q.run(key, "a")

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key, "a")) }
      } yield {}
      result.run()
    }

    test(s"$key: start, add, add, finish, fail, run") {
      val expected = key match {
        case Some(key) => List(Record.par((key, List("a", "c"))))
        case None      => List(Record.ser("a"), Record.ser("c"))
      }
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q(key) { d.get }

        b <- q(key) { error.raiseError[IO, String] }

        c <- q(key) { "c".pure[IO] }

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b.attempt
        _ <- IO { b shouldEqual error.asLeft }

        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"$key: run, run") {
      val expected = key match {
        case Some(key) => List(Record.par((key, List("a", "b"))))
        case None      => List(Record.ser("a"), Record.ser("b"))
      }
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key, "a")
        _ <- q.run(key, "b")

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"$key: add, add, run, run") {
      val expected = key match {
        case Some(key) => List(Record.par((key, List("a", "b"))))
        case None      => List(Record.ser("a"), Record.ser("b"))
      }
      val result = for {
        q <- Queue.of[IO, Int, String]
        d <- Deferred[IO, String]
        a <- q(key) { d.get }

        b <- q(key) { "b".pure[IO] }
        _ <- b.unfinished

        _ <- d.complete("a")

        a <- a
        _ <- IO { a shouldEqual "a" }
        b <- b
        _ <- IO { b shouldEqual "b" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"$key: start, add, finish, run") {
      val expected = key match {
        case Some(key) => List(Record.par((key, List("a", "b"))))
        case None      => List(Record.ser("a"), Record.ser("b"))
      }
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q.start(key) { d.get }

        b <- q(key) { "b".pure[IO] }
        _ <- b.unfinished

        _ <- d.complete("a")

        a <- a
        _ <- IO { a shouldEqual "a" }
        b <- b
        _ <- IO { b shouldEqual "b" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"$key: add, add, add, run, run, run") {
      val expected = key match {
        case Some(key) => List(Record.par((key, List("a", "b", "c"))))
        case None      => List(Record.ser("a"), Record.ser("b"), Record.ser("c"))
      }

      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q(key) { d.get }

        b <- q(key) { "b".pure[IO] }
        _ <- b.unfinished

        c <- q(key) { "c".pure[IO] }
        _ <- c.unfinished

        _ <- d.complete("a")

        a <- a
        _ <- IO { a shouldEqual "a" }
        b <- b
        _ <- IO { b shouldEqual "b" }
        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"$key: start, add, add, finish, run, run") {
      val expected = key match {
        case Some(key) => List(Record.par((key, List("a", "b", "c"))))
        case None      => List(Record.ser("a"), Record.ser("b"), Record.ser("c"))
      }

      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q.start(key) { d.get }

        b <- q(key) { "b".pure[IO] }
        c <- q(key) { "c".pure[IO] }

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }
        b <- b
        _ <- IO { b shouldEqual "b" }
        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }
  }

  for {
    (key0, key1) <- List((none, 0.some), (0.some, none))
  } yield {

    test(s"run $key0, run $key1") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key0, "a")
        _ <- q.run(key1, "b")

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key0, "a"), Record(key1, "b")) }
      } yield {}
      result.run()
    }

    test(s"add $key0, add $key1, run $key0, run $key1") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q(key0) { d.get }

        b <- q(key1) { "b".pure[IO] }
        _ <- b.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key0, "a"), Record(key1, "b")) }
      } yield {}
      result.run()
    }

    test(s"start $key0, add $key1, finish $key0, run $key1") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q.start(key0) { d.get }

        b <- q(key1) { "b".pure[IO] }
        _ <- b.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key0, "a"), Record(key1, "b")) }
      } yield {}
      result.run()
    }

    test(s"run $key0, run 2x$key1") {
      val expected = {
        val records = key1 match {
          case Some(key) => Record.par((key, List("b", "c"))) :: Nil
          case None      => Record.ser("b") :: Record.ser("c") :: Nil
        }
        Record(key0, "a") :: records
      }
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key0, "a")
        _ <- q.run(key1, "b")
        _ <- q.run(key1, "c")

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"add $key0, add 2x$key1, run $key0, run 2x$key1") {
      val expected = {
        val records = key1 match {
          case Some(key) => Record.par((key, List("b", "c"))) :: Nil
          case None      => Record.ser("b") :: Record.ser("c") :: Nil
        }
        Record(key0, "a") :: records
      }
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q(key0) { d.get }

        b <- q(key1) { "b".pure[IO] }
        _ <- b.unfinished

        c <- q(key1) { "c".pure[IO] }
        _ <- b.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"start $key0, add 2x$key1, finish $key0, run 2x$key1") {

      val expected = {
        val records = key1 match {
          case Some(key) => Record.par((key, List("b", "c"))) :: Nil
          case None      => Record.ser("b") :: Record.ser("c") :: Nil
        }
        Record(key0, "a") :: records
      }

      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q.start(key0) { d.get }

        b <- q(key1) { "b".pure[IO] }
        _ <- b.unfinished

        c <- q(key1) { "c".pure[IO] }
        _ <- c.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"run 2x$key0, run $key1") {
      val expected = {
        val records = key0 match {
          case Some(key) => Record.par((key, List("a", "b"))) :: Nil
          case None      => Record.ser("a") :: Record.ser("b") :: Nil
        }
        records :+ Record(key1, "c")
      }
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key0, "a")
        _ <- q.run(key0, "b")
        _ <- q.run(key1, "c")

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"add 2x$key0, add $key1, run 2x$key0, run $key1") {
      val expected = {
        val records = key0 match {
          case Some(key) => Record.par((key, List("a", "b"))) :: Nil
          case None      => Record.ser("a") :: Record.ser("b") :: Nil
        }
        records :+ Record(key1, "c")
      }
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q(key0) { d.get }

        b <- q(key0) { "b".pure[IO] }
        _ <- b.unfinished

        c <- q(key1) { "c".pure[IO] }
        _ <- b.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"start $key0, add $key0, add $key1, finish $key0, run $key0, run $key1") {

      val expected = {
        val records = key0 match {
          case Some(key) => Record.par((key, List("a", "b"))) :: Nil
          case None      => Record.ser("a") :: Record.ser("b") :: Nil
        }
        records :+ Record(key1, "c")
      }

      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q.start(key0) { d.get }

        b <- q(key0) { "b".pure[IO] }
        _ <- b.unfinished

        c <- q(key1) { "c".pure[IO] }
        _ <- c.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"add $key0, add $key1, add $key0, run $key0, run $key1, run $key0") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q(key0) { d.get }

        b <- q(key1) { "b".pure[IO] }
        _ <- b.unfinished

        c <- q(key0) { "c".pure[IO] }
        _ <- c.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key0, "a"), Record(key1, "b"), Record(key0, "c")) }
      } yield {}
      result.run()
    }

    test(s"start $key0, add $key1, add $key0, finish $key0, run $key1, run $key0") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        a <- q.start(key0) { d.get }

        b <- q(key1) { "b".pure[IO] }
        _ <- b.unfinished

        c <- q(key0) { "c".pure[IO] }
        _ <- c.unfinished

        _ <- d.complete("a")
        a <- a
        _ <- IO { a shouldEqual "a" }

        b <- b
        _ <- IO { b shouldEqual "b" }

        c <- c
        _ <- IO { c shouldEqual "c" }

        rs <- q.records
        _  <- IO { rs shouldEqual List(Record(key0, "a"), Record(key1, "b"), Record(key0, "c")) }
      } yield {}
      result.run()
    }

    test(s"run 3x$key0, run 3x$key1, run 3x$key0") {
      val expected = {
        def records(key: Option[Int], values: String*) = key match {
          case Some(key) => Record.par((key, values.toList)) :: Nil
          case None      => values.toList.map { value => Record.ser(value) }
        }
        records(key0, "a", "b", "c") ++ records(key1, "d", "e", "f") ++ records(key0, "g", "h", "i")
      }

      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key0, "a")
        _ <- q.run(key0, "b")
        _ <- q.run(key0, "c")

        _ <- q.run(key1, "d")
        _ <- q.run(key1, "e")
        _ <- q.run(key1, "f")

        _ <- q.run(key0, "g")
        _ <- q.run(key0, "h")
        _ <- q.run(key0, "i")

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"add 3x$key0, add 3x$key1, add 3x$key0, run 3x$key0, run 3x$key1, run 3x$key0") {
      val expected = {
        def records(key: Option[Int], values: String*) = key match {
          case Some(key) => Record.par((key, values.toList)) :: Nil
          case None      => values.toList.map { value => Record.ser(value) }
        }
        records(key0, "a", "b", "c") ++ records(key1, "d", "e", "f") ++ records(key0, "g", "h", "i")
      }

      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        _ <- q(key0) { d.get }
        _ <- q(key0) { "b".pure[IO] }
        c <- q(key0) { "c".pure[IO] }
        _ <- c.unfinished

        _ <- q(key1) { "d".pure[IO] }
        _ <- q(key1) { "e".pure[IO] }
        f <- q(key1) { "f".pure[IO] }
        _ <- f.unfinished

        _ <- q(key0) { "g".pure[IO] }
        _ <- q(key0) { "h".pure[IO] }
        i <- q(key0) { "i".pure[IO] }
        _ <- i.unfinished

        _ <- d.complete("a")

        c <- c
        _ <- IO { c shouldEqual "c" }

        f <- f
        _ <- IO { f shouldEqual "f" }

        i <- i
        _ <- IO { i shouldEqual "i" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }

    test(s"start $key0, add 2x$key0, add 3x$key1, add 3x$key0, run 3x$key0, run 3x$key1, run 3x$key0") {
      val expected = {
        def records(key: Option[Int], values: String*) = key match {
          case Some(key) => Record.par((key, values.toList)) :: Nil
          case None      => values.toList.map { value => Record.ser(value) }
        }
        records(key0, "a", "b", "c") ++ records(key1, "d", "e", "f") ++ records(key0, "g", "h", "i")
      }

      val result = for {
        q <- Queue.of[IO, Int, String]

        d <- Deferred[IO, String]
        _ <- q.start(key0) { d.get }
        _ <- q(key0) { "b".pure[IO] }
        c <- q(key0) { "c".pure[IO] }
        _ <- c.unfinished

        _ <- q(key1) { "d".pure[IO] }
        _ <- q(key1) { "e".pure[IO] }
        f <- q(key1) { "f".pure[IO] }
        _ <- f.unfinished

        _ <- q(key0) { "g".pure[IO] }
        _ <- q(key0) { "h".pure[IO] }
        i <- q(key0) { "i".pure[IO] }
        _ <- i.unfinished

        _ <- d.complete("a")

        c <- c
        _ <- IO { c shouldEqual "c" }

        f <- f
        _ <- IO { f shouldEqual "f" }

        i <- i
        _ <- IO { i shouldEqual "i" }

        rs <- q.records
        _  <- IO { rs shouldEqual expected }
      } yield {}
      result.run()
    }
  }

  val permutations =
    List(none, none, 0.some, 1.some)
      .permutations
      .toList
  permutations.map(xs =>
    (xs: @unchecked) match {
      case List(key0, key1, key2, key3) =>
        val expected = (key0, key1, key2, key3) match {
          case (Some(k0), Some(k1), Some(k2), Some(k3)) =>
            List(Record.par((k0, List("a")), (k1, List("b")), (k2, List("c")), (k3, List("d"))))

          case (None, Some(k1), Some(k2), Some(k3)) =>
            List(Record.ser("a"), Record.par((k1, List("b")), (k2, List("c")), (k3, List("d"))))

          case (Some(k0), None, Some(k2), Some(k3)) =>
            List(Record.par((k0, List("a"))), Record.ser("b"), Record.par((k2, List("c")), (k3, List("d"))))

          case (Some(k0), Some(k1), None, Some(k3)) =>
            List(Record.par((k0, List("a")), (k1, List("b"))), Record.ser("c"), Record.par((k3, List("d"))))

          case (Some(k0), None, None, Some(k3)) =>
            List(Record.par((k0, List("a"))), Record.ser("b"), Record.ser("c"), Record.par((k3, List("d"))))

          case (None, Some(k1), None, Some(k3)) =>
            List(Record.ser("a"), Record.par((k1, List("b"))), Record.ser("c"), Record.par((k3, List("d"))))

          case (None, None, Some(k2), Some(k3)) =>
            List(Record.ser("a"), Record.ser("b"), Record.par((k2, List("c")), (k3, List("d"))))

          case (None, None, None, Some(k3)) =>
            List(Record.ser("a"), Record.ser("b"), Record.ser("c"), Record.par((k3, List("d"))))

          case (Some(k0), Some(k1), Some(k2), None) =>
            List(Record.par((k0, List("a")), (k1, List("b")), (k2, List("c"))), Record.ser("d"))

          case (None, Some(k1), Some(k2), None) =>
            List(Record.ser("a"), Record.par((k1, List("b")), (k2, List("c"))), Record.ser("d"))

          case (Some(k0), None, Some(k2), None) =>
            List(Record.par((k0, List("a"))), Record.ser("b"), Record.par((k2, List("c"))), Record.ser("d"))

          case (Some(k0), Some(k1), None, None) =>
            List(Record.par((k0, List("a")), (k1, List("b"))), Record.ser("c"), Record.ser("d"))

          case (Some(k0), None, None, None) =>
            List(Record.par((k0, List("a"))), Record.ser("b"), Record.ser("c"), Record.ser("d"))

          case (None, Some(k1), None, None) =>
            List(Record.ser("a"), Record.par((k1, List("b"))), Record.ser("c"), Record.ser("d"))

          case (None, None, Some(k2), None) =>
            List(Record.ser("a"), Record.ser("b"), Record.par((k2, List("c"))), Record.ser("d"))

          case (None, None, None, None) =>
            List(Record.ser("a"), Record.ser("b"), Record.ser("c"), Record.ser("d"))
        }

        test(s"run $key0, $key1, $key2, $key3") {
          val result = for {
            q <- Queue.of[IO, Int, String]

            _ <- q.run(key0, "a")
            _ <- q.run(key1, "b")
            _ <- q.run(key2, "c")
            _ <- q.run(key3, "d")

            rs <- q.records
            _  <- IO { rs shouldEqual expected }
          } yield {}
          result.run()
        }

        test(s"add & run $key0, $key1, $key2, $key3") {
          val result = for {
            q <- Queue.of[IO, Int, String]

            d <- Deferred[IO, String]
            a <- q(key0) { d.get }

            b <- q(key1) { "b".pure[IO] }

            c <- q(key2) { "c".pure[IO] }

            e <- q(key3) { "d".pure[IO] }

            _ <- d.complete("a")
            a <- a
            _ <- IO { a shouldEqual "a" }

            b <- b
            _ <- IO { b shouldEqual "b" }

            c <- c
            _ <- IO { c shouldEqual "c" }

            e <- e
            _ <- IO { e shouldEqual "d" }

            rs <- q.records
            _  <- IO { rs shouldEqual expected }
          } yield {}
          result.run()
        }

        test(s"start & finish $key0, $key1, $key2, $key3") {
          val result = for {
            q <- Queue.of[IO, Int, String]

            d <- Deferred[IO, String]
            a <- q.start(key0) { d.get }

            b <- q(key1) { "b".pure[IO] }

            c <- q(key2) { "c".pure[IO] }

            e <- q(key3) { "d".pure[IO] }

            _ <- d.complete("a")
            a <- a
            _ <- IO { a shouldEqual "a" }

            b <- b
            _ <- IO { b shouldEqual "b" }

            c <- c
            _ <- IO { c shouldEqual "c" }

            e <- e
            _ <- IO { e shouldEqual "d" }

            rs <- q.records
            _  <- IO { rs shouldEqual expected }
          } yield {}
          result.run()
        }
    }
  )

  test("par, ser, par") {

    val expected = List(Record.ser("a"), Record.par((0, List("b", "e")), (1, List("c", "d"))))

    val result = for {
      q <- Queue.of[IO, Int, String]

      d0 <- Deferred[IO, String]
      _  <- q.start(none) { d0.get }

      d1 <- Deferred[IO, String]
      b  <- q(0.some) { d1.get }

      c <- q(1.some) { "c".pure[IO] }

      _ <- d0.complete("a")

      _ <- q.run(1.some, "d")

      c <- c
      _ <- IO { c shouldEqual "c" }

      e <- q(0.some) { "e".pure[IO] }

      _ <- d1.complete("b")
      b <- b
      _ <- IO { b shouldEqual "b" }

      e <- e
      _ <- IO { e shouldEqual "e" }

      rs <- q.records
      _  <- IO { rs shouldEqual expected }
    } yield {}
    result.run()
  }

  private implicit class Ops[F[_], A](val self: F[A]) {

    def unfinished(implicit async: Async[F]): F[Unit] = {
      for {
        a <- self.timeout(10.millis).attempt
        _ <- Sync[F].delay { a should matchPattern { case Left(_: TimeoutException) => () } }
      } yield ()
    }
  }

  private implicit class QueueOps[F[_], K, A](val self: Queue[F, K, A]) {

    def run(key: Option[K], a: A)(implicit F: Sync[F]): F[A] = {
      for {
        b <- self(key) { a.pure[F] }
        b <- b
        _ <- Sync[F].delay { b shouldEqual a }
      } yield b
    }
  }
}

object SerParQueueTest {

  sealed trait Record[+K, +V] {}

  object Record {

    def apply[K, V](key: Option[K], value: V): Record[K, V] = key match {
      case Some(key) => par((key, List(value)))
      case None      => ser(value)
    }

    def par[K, V](values: (K, List[V])*): Record[K, V] = Par(Map(values: _*))

    def ser[K, V](value: V): Record[K, V] = Ser(value)

    final case class Par[K, V](values: Map[K, List[V]]) extends Record[K, V]

    final case class Ser[V](value: V) extends Record[Nothing, V]
  }

  trait Records[F[_], K, V] {

    def add(key: Option[K], value: V): F[Unit]

    def get: F[List[Record[K, V]]]
  }

  object Records {

    def of[F[_]: Sync, K, V]: F[Records[F, K, V]] = {
      Ref[F]
        .of(List.empty[Record[K, V]])
        .map { ref =>
          new Records[F, K, V] {

            def add(key: Option[K], value: V) = {
              key.fold {
                ref.update { Record.Ser(value) :: _ }
              } { key =>
                ref.update {
                  case (r: Record.Par[K, V]) :: rs =>
                    val values = value :: r
                      .values
                      .getOrElse(key, List.empty)
                    val map = r
                      .values
                      .updated(key, values)
                    Record.Par(map) :: rs

                  case rs => Record.Par(Map((key, List(value)))) :: rs
                }
              }
            }

            def get = {
              ref
                .get
                .map { records =>
                  records.foldLeft(List.empty[Record[K, V]]) {
                    case (as, a: Record.Ser[V]) => a :: as
                    case (as, Record.Par(vs))   => Record.Par(vs.map { case (k, v) => k -> v.reverse }) :: as
                  }
                }
            }
          }
        }
    }
  }

  trait Queue[F[_], K, V] {

    def apply(key: Option[K])(task: F[V]): F[F[V]]

    def records: F[List[Record[K, V]]]
  }

  object Queue {

    def of[F[_]: Async: Parallel, K, A]: F[Queue[F, K, A]] = {
      for {
        queue    <- SerParQueue.of[F, K]
        records0 <- Records.of[F, K, A]
      } yield {
        new Queue[F, K, A] {
          def apply(key: Option[K])(task: F[A]) = {
            queue(key) { task.flatTap { a => records0.add(key, a) } }
          }
          def records = records0.get
        }
      }
    }

    implicit class QueueOpsSerParQueueTest[F[_], K, A](val self: Queue[F, K, A]) extends AnyVal {

      def start(key: Option[K])(task: F[A])(implicit F: Concurrent[F]): F[F[A]] = {
        for {
          d <- Deferred[F, Unit]
          a <- self(key) { d.complete(()) *> task }
        } yield a
      }
    }
  }
}
