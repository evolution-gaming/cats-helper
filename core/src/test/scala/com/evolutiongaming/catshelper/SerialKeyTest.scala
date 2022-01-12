package com.evolutiongaming.catshelper

import cats.data.{NonEmptyList => Nel}
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.effect.{Clock, Concurrent, IO, Sync, Temporal}
import cats.syntax.all._
import cats.Hash
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import cats.effect.kernel.Async

class SerialKeyTest extends AsyncFunSuite with Matchers {
  import SerialKeyTest._

  private val error: Throwable = new RuntimeException with NoStackTrace

  test("run in parallel for different keys & serially for same") {
    val result = for {
      q  <- Queue.of[IO, String, Int]
      da <- Deferred[IO, Int]
      _  <- q.start("a") { da.get }
      a  <- q("a") { 1.pure[IO] }

      db <- Deferred[IO, Int]
      _  <- q.start("b") { db.get }
      b  <- q("b") { 1.pure[IO] }
      _  <- db.complete(0)
      b  <- b
      _  <- IO { b shouldEqual 1 }

      _ <- da.complete(0)
      a <- a
      _ <- IO { a shouldEqual 1 }

      rs <- q.records
      _ <- IO { rs shouldEqual Map(("a", Nel.of(0, 1)), ("b", Nel.of(0, 1))) }
    } yield {}
    result.run()
  }

  test("enqueue is not async") {
    val threadId = IO { Thread.currentThread().getId }
    val result = for {
      q <- Queue.of[IO, String, Int]
      a <- threadId
      _ <- q("a") { 1.pure[IO] }
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
    key <- List(0)
  } yield {

    test("run") {
      val result = for {
        q <- Queue.of[IO, Int, String]
        _ <- q.run(key, "a")
        rs <- q.records
        _  <- IO { rs shouldEqual Map((key, Nel.of("a"))) }
      } yield {}
      result.run()
    }

    test("run, fail") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key, "a")

        a <- q(key) { error.raiseError[IO, String] }
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        rs <- q.records
        _  <- IO { rs shouldEqual Map((key, Nel.of("a"))) }
      } yield {}
      result.run()
    }

    test("start, add, fail, run") {
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
        _  <- IO { rs shouldEqual Map((key, Nel.of("b"))) }
      } yield {}
      result.run()
    }

    test("fail, run") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        a <- q(key) { error.raiseError[IO, String] }
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        _ <- q.run(key, "a")

        rs <- q.records
        _  <- IO { rs shouldEqual Map((key, Nel.of("a"))) }
      } yield {}
      result.run()
    }

    test("start, add, add, finish, fail, run") {
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
        _  <- IO { rs shouldEqual Map((key, Nel.of("a", "c"))) }
      } yield {}
      result.run()
    }

    test("run, run") {
      val result = for {
        q <- Queue.of[IO, Int, String]

        _ <- q.run(key, "a")
        _ <- q.run(key, "b")

        rs <- q.records
        _  <- IO { rs shouldEqual Map((key, Nel.of("a", "b"))) }
      } yield {}
      result.run()
    }

    test("add, add, run, run") {
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
        _  <- IO { rs shouldEqual Map((key, Nel.of("a", "b"))) }
      } yield {}
      result.run()
    }

    test("start, add, finish, run") {
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
        _  <- IO { rs shouldEqual Map((key, Nel.of("a", "b"))) }
      } yield {}
      result.run()
    }

    test("add, add, add, run, run, run") {
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
        _  <- IO { rs shouldEqual Map((key, Nel.of("a", "b", "c"))) }
      } yield {}
      result.run()
    }

    test("start, add, add, finish, run, run") {
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
        _  <- IO { rs shouldEqual Map((key, Nel.of("a", "b", "c"))) }
      } yield {}
      result.run()
    }
  }


  private implicit class Ops[F[_], A](val self: F[A]) {

    def unfinished(implicit sync: Async[F]): F[Unit] = {
      for {
        a <- self.timeout(10.millis).attempt
        _ <- Sync[F].delay { a should matchPattern { case Left(_: TimeoutException) => () } }
      } yield ()
    }
  }

  private implicit class QueueOps[F[_], K, A](val self: Queue[F, K, A]) {

    def run(key: K, a: A)(implicit F: Sync[F]): F[A] = {
      for {
        b <- self(key) { a.pure[F] }
        b <- b
        _ <- Sync[F].delay { b shouldEqual a }
      } yield b
    }
  }
}

object SerialKeyTest {

  trait Records[F[_], K, V] {

    def add(key: K, value: V): F[Unit]

    def get: F[Map[K, Nel[V]]]
  }

  object Records {

    def of[F[_]: Sync, K, V]: F[Records[F, K, V]] = {
      Ref[F]
        .of(Map.empty[K, Nel[V]])
        .map { ref =>
          new Records[F, K, V] {

            def add(key: K, value: V) = {
              ref.update { map =>
                val values = map.get(key) match {
                  case Some(values) => value :: values
                  case None => Nel.of(value)
                }
                map.updated(key, values)
              }
            }

            def get = {
              ref
                .get
                .map { _.map { case (key, values) => (key, values.reverse)} }
            }
          }
        }
    }
  }

  trait Queue[F[_], K, V] {

    def apply(key: K)(task: F[V]): F[F[V]]

    def records: F[Map[K, Nel[V]]]
  }

  object Queue {

    def of[F[_]: Async, K: Hash, A]: F[Queue[F, K, A]] = {
      for {
        queue    <- SerialKey.of[F, K]
        records0 <- Records.of[F, K, A]
      } yield {
        new Queue[F, K, A] {
          def apply(key: K)(task: F[A]) = {
            queue(key) { task.flatTap { a => records0.add(key, a) } }
          }
          def records = records0.get
        }
      }
    }

    implicit class QueueOpsSerParQueueTest[F[_], K, A](val self: Queue[F, K, A]) extends AnyVal {

      def start(key: K)(task: F[A])(implicit F: Concurrent[F]): F[F[A]] = {
        for {
          d <- Deferred[F, Unit]
          a <- self(key) { d.complete(()) *> task }
        } yield a
      }
    }
  }
}
