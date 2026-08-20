package com.evolutiongaming.catshelper

import cats.Id
import cats.arrow.FunctionK
import cats.data.{NonEmptyList => Nel}
import cats.effect.kernel.{Deferred, Outcome, Ref}
import cats.effect.testkit.TestControl
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Temporal}
import cats.implicits._
import com.evolutiongaming.catshelper.testkit.PureTest.ioTest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.util.control.NoStackTrace

class GroupWithinSpec extends AnyFreeSpec with Matchers {

  implicit val ioRuntime: IORuntime = IORuntime.global

  "support settings = 0" in ioTest { env =>
    import env._
    `support settings = 0`[IO]
  }

  "collect until size reached" in ioTest { env =>
    import env._
    `collect until size reached`[IO]
  }

  "collect until deadline reached" in ioTest { env =>
    import env._
    `collect until deadline reached`[IO]
  }

  "consume on release" in ioTest { env =>
    import env._
    `consume on release`[IO]
  }

  "handler calls never overlap" in {
    val settings = GroupWithin.Settings(delay = 1.milli, size = 2)
    val elements = 2000

    val program = for {
      inFlight <- Ref[IO].of(0)
      overlaps <- Ref[IO].of(0)
      delivered <- Ref[IO].of(List.empty[Int])
      handler = (batch: Nel[Int]) =>
        for {
          entered <- inFlight.updateAndGet { _ + 1 }
          _ <- overlaps.update { _ + 1 }.whenA(entered > 1)
          _ <- delivered.update { batch.toList ::: _ }
          _ <- IO.cede
          _ <- inFlight.update { _ - 1 }
        } yield {}
      _ <- GroupWithin[IO]
        .apply[Int](settings) { handler }
        .use { enqueue => (1 to elements).toList.parTraverse_ { enqueue.apply } }
      overlaps <- overlaps.get
      delivered <- delivered.get
    } yield {
      overlaps shouldEqual 0
      delivered.sorted shouldEqual (1 to elements).toList
    }

    program.timeout(60.seconds).unsafeRunSync()
  }

  // Demonstrates that the order of batches is not guaranteed, as stated in the GroupWithin
  // Scaladoc. A fiber takes its batch out of the Ref and only then acquires the semaphore, and
  // there is no async boundary between the two. A fiber that closes a later batch can therefore
  // reach the semaphore first, if the timer fiber is descheduled inside that window.
  //
  // Ignored because it needs real parallelism, which no test can force. A single enqueue run hits
  // the inversion in roughly one attempt in seven on a multi core machine, so the 1000 attempts
  // below detect it nearly always there, and never under TestControl, which is single threaded and
  // always lets a ready fiber finish before a sleeping one wakes. Un-ignore it to observe the
  // race.
  "batches are not delivered in order" ignore {
    val settings = GroupWithin.Settings(delay = 1.micro, size = 2)
    val elements = 2000
    val attempts = 1000

    val attempt = for {
      delivered <- Ref[IO].of(List.empty[Int])
      _ <- GroupWithin[IO]
        .apply[Int](settings) { batch => delivered.update { batch.toList.reverse ::: _ } }
        .use { enqueue => (1 to elements).toList.traverse_ { enqueue.apply } }
      observed <- delivered.get
    } yield observed.reverse

    val inversions = (1 to attempts).toList
      .traverse { _ => attempt.map { observed => observed != observed.sorted } }
      .unsafeRunSync()
      .count { identity }

    inversions should be > 0
  }

  "deliver a batch closed by the delay timer before release returns" in {
    val settings = GroupWithin.Settings(delay = 10.millis, size = 100)

    val program = for {
      delivered <- Ref[IO].of(List.empty[Int])
      started <- Deferred[IO, Unit]
      handler = (batch: Nel[Int]) =>
        started.complete(()) *> IO.sleep(1.second) *> delivered.update { batch.toList ::: _ }
      _ <- GroupWithin[IO]
        .apply[Int](settings) { handler }
        .use { enqueue => enqueue(1) *> enqueue(2) *> started.get }
      observed <- delivered.get
    } yield observed

    // The batching shape is not the point and is not stable: if more than `delay` elapses between
    // the two enqueues, the timer closes [1] on its own and [2] opens a second batch.
    program.timeout(30.seconds).unsafeRunSync().sorted shouldEqual List(1, 2)
  }

  "report an error raised while release flushes the pending batch" in {
    case object Error extends RuntimeException with NoStackTrace

    val settings = GroupWithin.Settings(delay = 1.day, size = 100)
    val program = GroupWithin[IO]
      .apply[Int](settings) { _ => IO.raiseError[Unit](Error) }
      .use { enqueue => enqueue(1) }
      .attempt

    program.timeout(30.seconds).unsafeRunSync() shouldEqual Error.asLeft
  }

  "not let a timer close a batch it was not started for" in {
    val settings = GroupWithin.Settings(delay = 1.day, size = 2)

    val program = for {
      delivered <- Ref[IO].of(List.empty[List[Int]])
      observed <- GroupWithin[IO]
        .apply[Int](settings) { batch => delivered.update { batch.toList :: _ } }
        .use { enqueue =>
          for {
            _ <- enqueue(1)
            _ <- enqueue(2)
            _ <- enqueue(3)
            _ <- IO.cede.replicateA_(10)
            observed <- delivered.get
          } yield observed
        }
    } yield observed

    // Under a simulated clock no time passes between the batches, so both get the same timestamp.
    // Only the batch closed by size may be delivered, element 3 is still accumulating.
    TestControl.executeEmbed(program).unsafeRunSync() shouldEqual List(List(1, 2))
  }

  "cancel the batch timer once the batch is consumed" in {
    val settings = GroupWithin.Settings(delay = 1.day, size = 2)
    val program = GroupWithin[IO]
      .apply[Int](settings) { _ => IO.unit }
      .use { enqueue => enqueue(1) *> enqueue(2) }

    val test = TestControl.execute(program).flatMap { control =>
      for {
        _ <- control.tick
        outcome <- control.results
        _ <- IO { outcome shouldEqual Outcome.succeeded[Id, Throwable, Unit](()).some }
        nextInterval <- control.nextInterval
      } yield {
        nextInterval shouldEqual Duration.Zero
      }
    }

    test.unsafeRunSync()
  }

  "enqueue at a constant cost for a large batch" in {
    val elements = 100000
    val settings = GroupWithin.Settings(delay = 1.day, size = elements + 1)
    val program = GroupWithin[IO]
      .apply[Int](settings) { _ => IO.unit }
      .use { enqueue => (1 to elements).toList.traverse_ { enqueue.apply } }

    program.timeout(10.seconds).unsafeRunSync()
  }

  private def `support settings = 0`[F[_]: Temporal] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 0)
    for {
      ref <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      _ <- groupWithin.use { enqueue0 =>
        val enqueue = enqueue0.mapK(FunctionK.id)
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
        } yield {}
      }
      a <- ref.get
    } yield {
      a shouldEqual List(Nel.of(2), Nel.of(1))
    }
  }

  private def `collect until size reached`[F[_]: Temporal] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 2)
    for {
      ref <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      _ <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
          _ <- enqueue(3)
          _ <- enqueue(4)
        } yield {}
      }
      a <- ref.get
    } yield {
      a shouldEqual List(Nel.of(3, 4), Nel.of(1, 2))
    }
  }

  private def `collect until deadline reached`[F[_]: Temporal] = {
    val delay = 1.minute
    val settings = GroupWithin.Settings(delay = delay, size = 100)
    for {
      ref <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      a <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
          // 1.nano is needed to avoid a race between end-of-group and the subsequent elements
          _ <- Temporal[F].sleep(delay + 1.nano)
          _ <- enqueue(3)
          _ <- enqueue(4)
          _ <- Temporal[F].sleep(delay + 1.nano)
          _ <- enqueue(5) // this won't be seen yet
          a <- ref.get
        } yield a
      }
    } yield {
      a shouldEqual List(Nel.of(3, 4), Nel.of(1, 2))
    }
  }

  private def `consume on release`[F[_]: Temporal] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 100)
    for {
      deferred <- Deferred[F, Nel[Int]]
      groupWithin = GroupWithin[F].apply[Int](settings) { a => deferred.complete(a).void }
      _ <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
        } yield {}
      }
      a <- deferred.get
    } yield {
      a shouldEqual Nel.of(1, 2)
    }
  }
}
