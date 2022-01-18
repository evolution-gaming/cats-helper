package com.evolutiongaming.catshelper

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Ref, Resource}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class ResourceRegistrySpec extends AnyFreeSpec with Matchers {

  implicit val ioRuntime: IORuntime = IORuntime.global

  "register resources and release them all" in {
    val io = for {
      allocated <- Ref[IO].of(0)
      released  <- Ref[IO].of(0)
      resource  = Resource.make(allocated.update(_ + 1))(_ => released.update(_ + 1))
      allocated <- ResourceRegistry[IO].use { manager =>
        for {
          _         <- manager.register(resource)
          _         <- manager.register(resource)
          _         <- manager.register(resource)
          allocated <- allocated.get
        } yield allocated
      }
      released <- released.get
    } yield allocated -> released

    val (allocated, released) = io.unsafeRunSync()
    allocated shouldBe 3
    released shouldBe 3
  }

  "survive after resource' allocation failure" in {
    val io = for {
      allocated <- Ref[IO].of(0)
      released  <- Ref[IO].of(0)
      resource  = Resource.make(allocated.update(_ + 1))(_ => released.update(_ + 1))
      failed    = Resource.make(IO.raiseError(new Exception))(_ => released.update(_ + 1))
      allocated <- ResourceRegistry[IO].use { manager =>
        for {
          _         <- manager.register(resource)
          _         <- manager.register(failed).attempt
          _         <- manager.register(resource)
          allocated <- allocated.get
        } yield allocated
      }
      released <- released.get
    } yield allocated -> released

    val (allocated, released) = io.unsafeRunSync()
    allocated shouldBe 2
    released shouldBe 2
  }

  "survive after resource' release failure" in {
    val io = for {
      allocated <- Ref[IO].of(0)
      released  <- Ref[IO].of(0)
      resource  = Resource.make(allocated.update(_ + 1))(_ => released.update(_ + 1))
      failed    = Resource.make(allocated.update(_ + 1))(_ => IO.raiseError(new Exception))
      allocated <- ResourceRegistry[IO].use { manager =>
        for {
          _         <- manager.register(resource)
          _         <- manager.register(failed)
          _         <- manager.register(resource)
          allocated <- allocated.get
        } yield allocated
      }
      released <- released.get
    } yield allocated -> released

    val (allocated, released) = io.unsafeRunSync()
    allocated shouldBe 3
    released shouldBe 2
  }

  "raise exception on using ResourceManager out of it's Resource scope" in {
    assertThrows[ResourceRegistry.AlreadyReleasedException.type] {
      ResourceRegistry[IO]
        .use(m => IO.pure(m))
        .flatMap(_.register(Resource.pure(42)))
        .unsafeRunSync()
    }
  }
}
