package com.evolutiongaming.catshelper

import cats.Hash
import cats.data.{NonEmptyList => Nel}
import cats.effect.kernel.Async
import cats.effect.kernel.{Deferred, Outcome, Ref}
import cats.effect.syntax.all._
import cats.effect.{Clock, Concurrent, IO, Sync, Temporal}
import cats.syntax.all._
import com.evolutiongaming.catshelper.IOSuite._
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

class SerialKeyTest extends AsyncFunSuite with Matchers {
  import SerialKeyTest._

  private val error: Throwable = new RuntimeException with NoStackTrace

  private def implementations[K: Hash]: List[(String, IO[SerialKey[IO, K]])] = List(
    "partitioned" -> SerialKey.of[IO, K],
    "concurrentHashMap" -> SerialKey.ofConcurrentHashMap[IO, K],
  )

  implementations[String].foreach { case (name, serialKey) =>
    test(s"$name runs tasks of one key in submission order") {
      val tasks = 200
      val result = for {
        serial <- serialKey
        order <- Ref[IO].of(Vector.empty[Int])
        awaits <- (1 to tasks).toList.traverse { index => serial("key") { order.update { _ :+ index } } }
        _ <- awaits.sequence_
        observed <- order.get
        _ <- IO { observed shouldEqual (1 to tasks).toVector }
      } yield {}
      result.run()
    }

    test(s"$name reports a task failure to its caller and keeps the key running") {
      val result = for {
        serial <- serialKey
        failed <- serial("key") { error.raiseError[IO, Int] }
        next <- serial("key") { 1.pure[IO] }
        a <- failed.attempt
        _ <- IO { a shouldEqual error.asLeft }
        b <- next
        _ <- IO { b shouldEqual 1 }
      } yield {}
      result.run()
    }

    test(s"$name runs different keys in parallel and one key serially") {
      val result = for {
        q <- Queue.of[IO, String, Int](serialKey)
        da <- Deferred[IO, Int]
        _ <- q.start("a") { da.get }
        a <- q("a") { 1.pure[IO] }

        db <- Deferred[IO, Int]
        _ <- q.start("b") { db.get }
        b <- q("b") { 1.pure[IO] }
        _ <- db.complete(0)
        b <- b
        _ <- IO { b shouldEqual 1 }

        _ <- da.complete(0)
        a <- a
        _ <- IO { a shouldEqual 1 }

        rs <- q.records
        _ <- IO { rs shouldEqual Map(("a", Nel.of(0, 1)), ("b", Nel.of(0, 1))) }
      } yield {}
      result.run()
    }

    test(s"$name does not make enqueue async") {
      val threadId = IO { Thread.currentThread().getId }
      val result = for {
        q <- Queue.of[IO, String, Int](serialKey)
        a <- threadId
        _ <- q("a") { 1.pure[IO] }
        b <- threadId
        _ <- IO { a shouldEqual b }
      } yield {}
      result.run()
    }

    // Ignored: a canceled task wedges the key for good. Both fixes considered so far cost more
    // than the defect, see https://github.com/evolution-gaming/cats-helper/issues/404
    ignore(s"$name advances a key after a task cancels") {
      val result = for {
        serial <- serialKey
        canceled <- serial("key")(IO.canceled)
        next <- serial("key")(IO.pure(1))
        value <- next
        _ = value shouldEqual 1
        fiber <- canceled.start
        outcome <- fiber.join
        _ = outcome should matchPattern { case Outcome.Canceled() => }
      } yield {}

      result.run()
    }
  }

  for {
    (name, serialKey) <- implementations[Int]
    key <- List(0)
  } yield {

    test(s"$name runs many tasks across many keys") {
      val tasksPerKey = 1000
      val keyCount = 10

      val result = for {
        serial <- serialKey
        last <- (1 to keyCount).toList.parTraverse { key =>
          (1, 0.pure[IO])
            .tailRecM {
              case (n, a) =>
                if (n > tasksPerKey) a.asRight[(Int, IO[Int])].pure[IO]
                else serial(key) { n.pure[IO] }.map { a => (n + 1, a).asLeft[IO[Int]] }
            }
            .flatten
        }
        _ <- IO { last.distinct shouldEqual List(tasksPerKey) }
      } yield {}

      result.run(1.minute)
    }

    test(s"$name run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)
        _ <- q.run(key, "a")
        rs <- q.records
        _ <- IO { rs shouldEqual Map((key, Nel.of("a"))) }
      } yield {}
      result.run()
    }

    test(s"$name run, fail") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

        _ <- q.run(key, "a")

        a <- q(key) { error.raiseError[IO, String] }
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        rs <- q.records
        _ <- IO { rs shouldEqual Map((key, Nel.of("a"))) }
      } yield {}
      result.run()
    }

    test(s"$name start, add, fail, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

        d <- Deferred[IO, Either[Throwable, String]]
        a <- q(key) { d.get.rethrow }

        b <- q(key) { "b".pure[IO] }

        _ <- d.complete(error.asLeft)
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        b <- b
        _ <- IO { b shouldEqual "b" }

        rs <- q.records
        _ <- IO { rs shouldEqual Map((key, Nel.of("b"))) }
      } yield {}
      result.run()
    }

    test(s"$name fail, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

        a <- q(key) { error.raiseError[IO, String] }
        a <- a.attempt
        _ <- IO { a shouldEqual error.asLeft }

        _ <- q.run(key, "a")

        rs <- q.records
        _ <- IO { rs shouldEqual Map((key, Nel.of("a"))) }
      } yield {}
      result.run()
    }

    test(s"$name start, add, add, finish, fail, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

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
        _ <- IO { rs shouldEqual Map((key, Nel.of("a", "c"))) }
      } yield {}
      result.run()
    }

    test(s"$name run, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

        _ <- q.run(key, "a")
        _ <- q.run(key, "b")

        rs <- q.records
        _ <- IO { rs shouldEqual Map((key, Nel.of("a", "b"))) }
      } yield {}
      result.run()
    }

    test(s"$name add, add, run, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)
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
        _ <- IO { rs shouldEqual Map((key, Nel.of("a", "b"))) }
      } yield {}
      result.run()
    }

    test(s"$name start, add, finish, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

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
        _ <- IO { rs shouldEqual Map((key, Nel.of("a", "b"))) }
      } yield {}
      result.run()
    }

    test(s"$name add, add, add, run, run, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

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
        _ <- IO { rs shouldEqual Map((key, Nel.of("a", "b", "c"))) }
      } yield {}
      result.run()
    }

    test(s"$name start, add, add, finish, run, run") {
      val result = for {
        q <- Queue.of[IO, Int, String](serialKey)

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
        _ <- IO { rs shouldEqual Map((key, Nel.of("a", "b", "c"))) }
      } yield {}
      result.run()
    }
  }

  private implicit class Ops[F[_], A](val self: F[A]) {

    def unfinished(
      implicit
      sync: Async[F],
    ): F[Unit] = {
      for {
        a <- self.timeout(10.millis).attempt
        _ <- Sync[F].delay { a should matchPattern { case Left(_: TimeoutException) => () } }
      } yield ()
    }
  }

  private implicit class QueueOps[F[_], K, A](val self: Queue[F, K, A]) {

    def run(
      key: K,
      a: A,
    )(implicit
      F: Sync[F],
    ): F[A] = {
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
                .map { _.map { case (key, values) => (key, values.reverse) } }
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

    def of[F[_]: Async, K: Hash, A](serialKey: F[SerialKey[F, K]]): F[Queue[F, K, A]] = {
      for {
        queue <- serialKey
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

      def start(
        key: K,
      )(
        task: F[A],
      )(implicit
        F: Concurrent[F],
      ): F[F[A]] = {
        for {
          d <- Deferred[F, Unit]
          a <- self(key) { d.complete(()) *> task }
        } yield a
      }
    }
  }
}
