package com.evolutiongaming.catshelper

import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.all._
import com.evolutiongaming.catshelper.testkit.PureTest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ReadWriteRefSpec extends AnyFreeSpec {
  "basic read/write" in scope { s =>
    import s._
    (read, inc, read, inc, read).tupled.map(_ shouldBe ((0, (), 1, (), 2)))
  }

  "enforce fair execution" in scope { s =>
    import s._, env._
    // We're going to start a bunch of reads and writes so that
    //  - they all get "scheduled" before any of them completes;
    //  - they all have a short delay to have a predictable scheduling order.
    // We will measure the time when each operation starts to check for the fairness.

    // Note that this test case also covers parallelism of `read` and exclusiveness of `wright`.

    val dr = 10.millis  // read duration
    val dw = 1.second   // write duration
    val ds = 1.nano     // scheduling delay duration

    val r: IO[(Int, FiniteDuration)] =
      rw.read.use(i => (IO.pure(i), getTime).tupled <* IO.sleep(dr))

    val w: IO[(Int, FiniteDuration)] =
      rw.write.use(upd => (upd.plain(_ + 1), getTime).tupled <* IO.sleep(dw))

    List(r, r, w, r, w, w, r, r)
      .traverse(op => op.start <* IO.sleep(ds))
      .flatMap(_.traverse(_.join))
      .map(_ shouldBe List(
        (0, 0.nanos),
        (0, ds), // here we have our scheduling delay
        (1, ds + dr), // this is our first write started after both reads complete
        (1, ds + dr + dw), // this is the next read
        (2, ds + dr + dw + dr), // write
        (3, ds + dr + dw + dr + dw), // another write
        (3, ds + dr + dw + dr + dw + dw), // last two …
        (3, ds + dr + dw + dr + dw + dw), // … concurrent reads
      ))
  }

  "can cancel blocked read" in scope { s =>
    import s._, env._

    val lateReadWithTimeout = IO.sleep(1.second) *> IO.race(read, IO.sleep(1.second) *> getTime)

    (incFor(1.minute), lateReadWithTimeout).parTupled.map(_ shouldBe (((), 2.seconds.asRight)))
  }

  "can cancel blocked write" in scope { s =>
    import s._, env._

    val lateIncWithTimeout = IO.sleep(1.second) *> IO.race(inc, IO.sleep(1.second) *> getTime)

    (incFor(1.minute), lateIncWithTimeout).parTupled.map(_ shouldBe (((), 2.seconds.asRight)))
  }

  "cancelled write unblocks pending reads" in scope { s =>
    import s._, env._

    val readForever = rw.read.use(_ => IO.never)
    val cancelTime = 1.minute
    val writeThatGetsCancelled = IO.race(inc, testRuntime.sleepUntil(cancelTime))

    IO.race(
      readForever.start *> IO.sleep(1.nano) *> writeThatGetsCancelled *> IO.never,
      IO.sleep(1.second) *> (read, getTime).tupled
    ).map(_ shouldBe Right((0, cancelTime)))

  }

  // This is a scary cases we might want to improve later.
  "read within write BLOCKS INDEFINITELY" in scope { s =>
    import s._, env._

    val writeInRead = rw.read.use(_ => inc)
    IO.race(writeInRead, IO.sleep(1.minute)).map(_ shouldBe ().asRight)
  }

  // This is a scary cases we might want to improve later.
  "read within read when write is pending BLOCKS INDEFINITELY" in scope { s =>
    import s._, env._

    val readInRead = rw.read.use(_ => inc.start *> IO.sleep(1.nano) *> read)
    IO.race(readInRead, IO.sleep(1.minute)).map(_ shouldBe ().asRight)
  }

  "race conditions" - {
    // Unfortunately PureTest can't catch every race-condition. E.g concurrent cancellation issues may
    // go undetected, since IO run loop may execute a cancellable batch as a single fused runnable.
    // Hence here we resort to actual multi-threaded executors for actual concurrency.

    final class Env(implicit val ec: ExecutionContext, val cs: ContextShift[IO], val timer: Timer[IO])
    val env = cats.effect.Resource {
      IO {
        val tp = java.util.concurrent.Executors.newFixedThreadPool(32)
        val ec = scala.concurrent.ExecutionContext.fromExecutor(tp)
        val env = new Env()(ec, IO.contextShift(ec), IO.timer(ec))
        env -> IO { tp.shutdown() }
      }
    }

    "read cancellation does not break things" in {
      env
        .use { env =>
          import env._

          for {
            rw <- ReadWriteRef[IO].of(1)
            _  <- {
              val tryReadAndCancel = rw.read.use(_ => IO.unit).start.flatMap(_.cancel) *> IO.shift
              val repeated = List.fill(1000)(tryReadAndCancel).sequence_
              List.fill(16)(repeated).parSequence_
            }
            _  <- rw.write.use(_ => IO.unit).timeout(100.millis)
          } yield ()
        }
        .unsafeRunTimed(10.seconds)
    }
  }

  private case class Scope(env: PureTest.Env[IO], rw: ReadWriteRef[IO, Int]) {
    import env._

    def incFor(duration: FiniteDuration): IO[Unit] =
      rw.write.use(upd => IO.sleep(duration) *> upd.plain(_ + 1).void)

    def inc: IO[Unit] =
      rw.write.use(_.plain(_ + 1).void)

    def read: IO[Int] = rw.read.use(IO.pure)

    def getTime: IO[FiniteDuration] = testRuntime.getTimeSinceStart
  }

  private def scope[A](body: Scope => IO[A]): A = PureTest.ioTest { env =>
    import env._
    for {
      rw <- ReadWriteRef[IO].of(0)
      a  <- body(Scope(env, rw))
    } yield a
  }
}
