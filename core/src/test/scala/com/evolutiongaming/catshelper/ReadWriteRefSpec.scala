package com.evolutiongaming.catshelper

import cats.effect.IO
import cats.implicits._
import com.evolutiongaming.catshelper.testkit.PureTest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.concurrent.duration._

class ReadWriteRefSpec extends AnyFreeSpec {
  "basic read/write" in scope { s =>
    import s._
    (read, inc, read, inc, read).tupled.map(_ shouldBe ((0, (), 1, (), 2)))
  }

  "allow concurrent reads" in scope { s =>
    import s._, env._
    val d = 1.minute
    val readAndGetTime = readFor(d) *> getTime
    (readAndGetTime, readAndGetTime).parTupled.map(_ shouldBe ((d, d)))
  }

  "allow writes only when unused" in scope { s =>
    import s._, env._

    val d = 1.minute

    // This read takes some time and then does an _inner_ read to make sure there was no increment.
    val readLong = rw.read.use(i => IO.sleep(d) *> read.tupleLeft(i))

    // For the write part we introduce some sleep to make sure read starts first,
    // and we track the time when it ends.
    val lateInc = sleepNs *> inc *> getTime

    // Double check the increment with a late read
    val sanityCheck = IO.sleep(d + d) *> read

    (readLong, lateInc, sanityCheck).parTupled.map(_ shouldBe (((0, 0), d, 1)))
  }

  "disallow reads during write" in scope { s =>
    import s._, env._

    val d = 1.minute

    // Race an increment with delayed read and check that read completes strictly after the increment.
    (incFor(d), sleepNs *> (read, getTime).tupled).parTupled.map(_ shouldBe (((), (1, d))))
  }

  "disallow write during another write" in scope { s =>
    import s._, env._

    val d = 1.minute

    // Late increment should wait for the first to complete
    val lateInc = sleepNs *> inc *> getTime

    val sanityCheck = IO.sleep(d + d) *> read

    (incFor(d), lateInc, sanityCheck).parTupled.map(_ shouldBe (((), d, 2)))
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

  // This is a scary case we might want to improve later.
  "upgrade read to write BLOCKS INDEFINITELY" in scope { s =>
    import s._, env._

    val upgrade = rw.read.use(_ => inc)
    IO.race(upgrade, IO.sleep(1.minute)).map(_ shouldBe ().asRight)
  }

  private case class Scope(env: PureTest.Env[IO], rw: ReadWriteRef[IO, Int]) {
    import env._

    def incFor(duration: FiniteDuration): IO[Unit] =
      rw.write.use(upd => IO.sleep(duration) *> upd.plain(_ + 1).void)

    def inc: IO[Unit] =
      rw.write.use(_.plain(_ + 1).void)

    def readFor(duration: FiniteDuration): IO[Int] = rw.read.use(i => IO.sleep(duration) as i)

    def read: IO[Int] = rw.read.use(IO.pure)

    def sleepNs: IO[Unit] = IO.sleep(1.nano)

    def getTime: IO[FiniteDuration] = testRuntime.getTimeSinceStart
  }

  private def scope[A](body: Scope => IO[A]): A = PureTest.ioTest.apply[A] { env =>
    import env._
    for {
      rw <- ReadWriteRef[IO].of(0)
      a  <- body(Scope(env, rw))
    } yield a
  }
}
