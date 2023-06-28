package com.evolution.catshelper

import cats.arrow.FunctionK
import cats.data.{NonEmptyList => Nel}
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Temporal}
import cats.implicits._
import com.evolution.catshelper.testkit.PureTest.ioTest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import com.evolution.catshelper.testkit.PureTest

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

  private def `support settings = 0`[F[_]: Temporal] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 0)
    for {
      ref         <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      _           <- groupWithin.use { enqueue0 =>
        val enqueue = enqueue0.mapK(FunctionK.id)
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
        } yield {}
      }
      a          <- ref.get
    } yield {
      a shouldEqual List(Nel.of(2), Nel.of(1))
    }
  }

  private def `collect until size reached`[F[_] : Temporal] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 2)
    for {
      ref         <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      _           <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
          _ <- enqueue(3)
          _ <- enqueue(4)
        } yield {}
      }
      a          <- ref.get
    } yield {
      a shouldEqual List(Nel.of(3, 4), Nel.of(1, 2))
    }
  }

  private def `collect until deadline reached`[F[_] : Temporal] = {
    val delay = 1.minute
    val settings = GroupWithin.Settings(delay = delay, size = 100)
    for {
      ref         <- Ref[F].of(List.empty[Nel[Int]])
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => ref.update { a :: _ } }
      a           <- groupWithin.use { enqueue =>
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

  private def `consume on release`[F[_] : Temporal] = {
    val settings = GroupWithin.Settings(delay = 1.minute, size = 100)
    for {
      deferred    <- Deferred[F, Nel[Int]]
      groupWithin  = GroupWithin[F].apply[Int](settings) { a => deferred.complete(a).void }
      _           <- groupWithin.use { enqueue =>
        for {
          _ <- enqueue(1)
          _ <- enqueue(2)
        } yield {}
      }
      a          <- deferred.get
    } yield {
      a shouldEqual Nel.of(1, 2)
    }
  }
}
