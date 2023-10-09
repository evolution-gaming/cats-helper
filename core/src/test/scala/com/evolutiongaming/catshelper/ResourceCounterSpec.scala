package com.evolutiongaming.catshelper

import cats.effect.std.CountDownLatch
import cats.effect.syntax.all.*
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Ref, Resource}
import com.evolutiongaming.catshelper.testkit.PureTest.ioTest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ResourceCounterSpec extends AnyFreeSpec with Matchers {
  implicit val ioRuntime: IORuntime = IORuntime.global

  "source resource allocated/released only once" in ioTest { env =>
    import env.*

    for {
      allocations <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      source = Resource.make[IO, Unit](allocations.update(_ + 1))(
        _ => releases.update(_ + 1)
      )

      counter <- ResourceCounter.of(source)

      release1 <- counter.resource.allocated.map(_._2)
      _ <- allocations.get.map(_ shouldBe 1)
      _ <- releases.get.map(_ shouldBe 0)

      release2 <- counter.resource.allocated.map(_._2)
      _ <- allocations.get.map(_ shouldBe 1)
      _ <- releases.get.map(_ shouldBe 0)

      _ <- release1
      _ <- allocations.get.map(_ shouldBe 1)
      _ <- releases.get.map(_ shouldBe 0)

      _ <- release2
      _ <- allocations.get.map(_ shouldBe 1)
      _ <- releases.get.map(_ shouldBe 1)
    } yield {}
  }

  "source resource allocated/released only once in concurrent usage" in ioTest {
    env =>
      import env.*

      val n = 10

      for {
        allocations <- Ref.of[IO, Int](0)
        releases <- Ref.of[IO, Int](0)
        source = Resource.make[IO, Unit](allocations.update(_ + 1))(
          _ => releases.update(_ + 1)
        )

        counter <- ResourceCounter.of(source)
        barrier <- CountDownLatch[IO](n)

        fiber <- Range(0, n).toVector
          .parTraverseN(32) { _ =>
            counter.resource.use { _ =>
              for {
                _ <- allocations.get.map(_ shouldBe 1)
                _ <- releases.get.map(_ shouldBe 0)
                _ <- barrier.release
                _ <- barrier.await
              } yield {}
            }
          }
          .start

        _ <- fiber.joinWithNever
        _ <- allocations.get.map(_ shouldBe 1)
        _ <- releases.get.map(_ shouldBe 1)
      } yield {}
  }

  "source resource can be re-allocated if it was previously released" in ioTest {
    env =>
      import env.*

      for {
        allocations <- Ref.of[IO, Int](0)
        releases <- Ref.of[IO, Int](0)
        source = Resource.make[IO, Unit](allocations.update(_ + 1))(
          _ => releases.update(_ + 1)
        )

        counter <- ResourceCounter.of(source)

        _ <- counter.resource.use { _ =>
          for {
            _ <- allocations.get.map(_ shouldBe 1)
            _ <- releases.get.map(_ shouldBe 0)
          } yield {}
        }

        _ <- allocations.get.map(_ shouldBe 1)
        _ <- releases.get.map(_ shouldBe 1)

        _ <- counter.resource.use { _ =>
          for {
            _ <- allocations.get.map(_ shouldBe 2)
            _ <- releases.get.map(_ shouldBe 1)
          } yield {}
        }

        _ <- allocations.get.map(_ shouldBe 2)
        _ <- releases.get.map(_ shouldBe 2)
      } yield {}
  }
}
