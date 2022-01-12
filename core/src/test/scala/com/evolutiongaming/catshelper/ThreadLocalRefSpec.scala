package com.evolutiongaming.catshelper

import java.util.concurrent.Executors

import cats.Parallel
import cats.arrow.FunctionK
import cats.effect._
import cats.effect.kernel.Ref
import cats.effect.unsafe._
import cats.implicits._
import com.evolutiongaming.catshelper.IOSuite._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.Succeeded

class ThreadLocalRefSpec extends AsyncFunSuite with Matchers {

  test("thread local stored per thread") {
    // here we override implicit IORuntime from TestIORuntime to guarantee that we have multiple threads
    // regardless of number of CPUs
    val (execContext, release) = executor[IO](5).allocated.unsafeRunSync()
    val (blocking, blockingSh) = IORuntime.createDefaultBlockingExecutionContext()
    val (scheduler, schedulerSh) = IORuntime.createDefaultScheduler()
    val runtime = IORuntime(
      compute = execContext,
      blocking = blocking,
      scheduler = scheduler,
      shutdown = () => {
        release.unsafeRunSync()
        blockingSh()
        schedulerSh()
      },
      config = IORuntimeConfig.apply()
    )

    testF[IO](5).timeout(5.seconds).as(Succeeded).unsafeToFuture()(runtime)
  }

  private def testF[F[_] : Async : ThreadLocalOf : Parallel](n: Int): F[Unit] = {

    def test(ref: ThreadLocalRef[F, String], executor: ExecutionContext) = {

      val get = ref.get

      val check = {
        for {
          a0 <- get
          a1 <- get
          _   = a0 shouldEqual a1
        } yield a0
      }

      for {
        a  <- check
        a1 <- Async[F].evalOn(get, executor)
        _   = a should not equal a1
        _  <- ref.set(a + "|")
        _  <- check
        _  <- ref.update(_ + "|")
        _  <- check
        _  <- ref.modify(a => (a + "|", ()))
        _  <- check
      } yield a
    }

    executor[F](parallelism = n).use { executor =>

      for {
        counter     <- Ref[F].of(0)
        thread       = for {
          _        <- counter.update(_ + 1)
          thread   <- Sync[F].delay { Thread.currentThread().toString }
        } yield thread
        threadLocal  <- ThreadLocalOf.summon[F].apply(thread)
        threadLocal1  = threadLocal.mapK(FunctionK.id)
        a             = test(threadLocal1, executor)
        treadIds     <- List.fill(n)(a).parSequence
        counter      <- counter.get
      } yield {
        val size = treadIds.distinct.size
        size should be > 1
        counter should be >= size
        ()
      }
    }
  }

  private def executor[F[_] : Sync](parallelism: Int): Resource[F, ExecutionContextExecutorService] = {
    val result = Sync[F].delay {
      val es = Executors.newFixedThreadPool(parallelism)
      val ec = ExecutionContext.fromExecutorService(es)
      val release = Sync[F].delay { ec.shutdown() }
      (ec, release)
    }
    Resource(result)
  }
}
