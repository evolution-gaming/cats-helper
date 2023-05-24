package com.evolutiongaming.catshelper
import cats.data.StateT
import cats.effect.{Clock, IO, Timer}
import cats.{Applicative, Id}
import cats.syntax.either._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import com.evolutiongaming.catshelper.syntax.measureDuration._
import java.util.concurrent.TimeUnit

import cats.effect.concurrent.Ref

import scala.concurrent.duration._

class MeasureDurationSpec extends AnyFunSuite with Matchers {

  import MeasureDurationSpec._

  test("measure duration") {
    val stateT = for {
      duration <- MeasureDuration[IdState].start
      duration <- duration
    } yield duration

    val (state, duration) = stateT.run(State(List(1, 3)))
    duration shouldEqual 2.nanos
    state shouldEqual State.Empty
  }

  test("MeasureDurationOps.measured") {
    val test = Timer[IdState].sleep(3.seconds).measured {
      time => StateT.modify(old => State(time.toNanos :: old.timestamps))
    }

    test.runS(State(List(0, 0))) shouldEqual State(3.seconds.toNanos :: Nil)
  }

  test("MeasureDurationOps.measuredCase success") {
    val test = Timer[StateT[Either[Throwable, *], State, *]]
      .sleep(3.seconds)
      .measuredCase(
        time => StateT.modify(old => State(time.toNanos +: old.timestamps)),
        _ => StateT.modify(old => State(-1L +: old.timestamps))
      )

    test.runS(State(List(0, 0))) shouldEqual State(3.seconds.toNanos :: Nil).asRight[Throwable]
  }

  test("MeasureDurationOps.measuredCase failure") {
    val test = for {
      ref    <- StateT.liftF(Ref.of[IO, List[FiniteDuration]](List.empty))
      _      <- StateT.liftF[IO, State, Unit](IO.raiseError(new RuntimeException("test"))).measuredCase(
        _    => StateT.liftF(ref.update(1.day :: _)),
        time => StateT.liftF(ref.update(time :: _))
      ).attempt
      time   <- StateT.liftF(ref.get)
    } yield time

    test.runA(State(List(0, 5))).unsafeRunSync() shouldEqual List(5.nanos)
  }

}

object MeasureDurationSpec {

  type IdState[A] = StateT[Id, State, A]

  final case class State(timestamps: List[Long]) {

    def timestamp: (State, Long) = {
      timestamps match {
        case a :: timestamps => (copy(timestamps = timestamps), a)
        case Nil             => (this, 0)
      }
    }
  }

  object State {
    val Empty: State = State(List.empty)
  }

  implicit def timerStateT[F[_]: Applicative]: Timer[StateT[F, State, *]] = new Timer[StateT[F, State, *]] {
    def clock: Clock[StateT[F, State, *]] = new Clock[StateT[F, State, *]] {

      def realTime(unit: TimeUnit): StateT[F, State, Long] =
        StateT { state =>
          val (state1, timestamp) = state.timestamp
          val timestamp1 = unit.convert(timestamp, TimeUnit.NANOSECONDS)
          (state1, timestamp1).pure[F]
        }

      def monotonic(unit: TimeUnit): StateT[F, State, Long] =
        StateT { state =>
          val (state1, timestamp) = state.timestamp
          val timestamp1 = unit.convert(timestamp, TimeUnit.NANOSECONDS)
          (state1, timestamp1).pure[F]
        }
    }

    def sleep(duration: FiniteDuration): StateT[F, State, Unit] =
      StateT.modify { state =>
        State(state.timestamps.map(_ + duration.toNanos))
      }

  }

}
