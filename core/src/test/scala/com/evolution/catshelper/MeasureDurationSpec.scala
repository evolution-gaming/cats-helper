package com.evolution.catshelper

import cats.data.StateT
import cats.effect.{Clock, IO}
import cats.{Applicative, Id, Monad}
import cats.syntax.either._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import com.evolution.catshelper.syntax.measureDuration._
import cats.effect.Ref
import cats.effect.unsafe.implicits.global

import scala.concurrent.duration._

class MeasureDurationSpec extends AnyFunSuite with Matchers {

  import MeasureDurationSpec._

  test("measure duration") {
    val stateT = for {
      duration <- MeasureDuration[IdState].start
      duration <- duration
    } yield duration

    val (state, duration) = stateT.run(State(List(1.nano, 3.nanos)))
    duration shouldEqual 2.nanos
    state shouldEqual State.Empty
  }

  test("MeasureDurationOps.measured") {
    val test = Sleep[IdState].sleep(3.seconds).measured {
      time => StateT.modify[Id, State](old => State(time :: old.timestamps))
    }

    test.runS(State(List(0.nano, 0.nano))) shouldEqual State(3.seconds :: Nil)
  }

  test("MeasureDurationOps.measuredCase success") {
    val test = Sleep[StateT[Either[Throwable, _], State, _]]
      .sleep(3.seconds)
      .measuredCase(
        time => StateT.modify[Either[Throwable, _], State](old => State(time +: old.timestamps)),
        _ => StateT.modify[Either[Throwable, _], State](old => State(-1.nano +: old.timestamps))
      )

    test.runS(State(List(0.nano, 0.nano))) shouldEqual State(3.seconds :: Nil).asRight[Throwable]
  }

  test("MeasureDurationOps.measuredCase failure") {
    val test = for {
      ref <- StateT.liftF(Ref.of[IO, List[FiniteDuration]](List.empty))
      _ <- StateT.liftF[IO, State, Unit](IO.raiseError(new RuntimeException("test"))).measuredCase(
        _ => StateT.liftF(ref.update(1.day :: _)),
        time => StateT.liftF(ref.update(time :: _))
      ).attempt
      time <- StateT.liftF(ref.get)
    } yield time

    test.runA(State(List(0.nano, 5.nanos))).unsafeRunSync() shouldEqual List(5.nanos)
  }

}

object MeasureDurationSpec {

  type IdState[A] = StateT[Id, State, A]

  final case class State(timestamps: List[FiniteDuration]) {

    def timestamp: (State, FiniteDuration) = {
      timestamps match {
        case a :: timestamps => (copy(timestamps = timestamps), a)
        case Nil => (this, 0.nano)
      }
    }
  }

  object State {
    val Empty: State = State(List.empty)
  }

  trait Sleep[F[_]] {
    def sleep(duration: FiniteDuration): F[Unit]
  }

  object Sleep {
    def apply[F[_]](implicit ev: Sleep[F]): Sleep[F] = ev
  }

  implicit def sleep[F[_]: Applicative]: Sleep[StateT[F, State, _]] ={
    new Sleep[StateT[F, State, _]] {
      override def sleep(duration: FiniteDuration): StateT[F, State, Unit] =
        StateT.modify { state =>
          State(state.timestamps.map(_ + duration))
        }
    }
  }

  implicit def clock[F[_] : Monad]: Clock[StateT[F, State, _]] =
    new Clock[StateT[F, State, _]] {
      override def realTime: StateT[F, State, FiniteDuration] =
        StateT { state =>
          val (state1, timestamp) = state.timestamp
          (state1, timestamp).pure[F]
        }

      def monotonic: StateT[F, State, FiniteDuration] =
        StateT { state =>
          val (state1, timestamp) = state.timestamp
          (state1, timestamp).pure[F]
        }

      override def applicative: Applicative[StateT[F, State, _]] =
        Applicative[StateT[F, State, _]]
    }
}
