package com.evolutiongaming.catshelper

import cats.arrow.FunctionK
import cats.effect.{Async, Sync}
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait FromFuture[F[_]] {

  def apply[A](future: => Future[A]): F[A]
}

object FromFuture {

  /** Convinient summon-and-run function
   *
   * @example {{{
   *          class Foo[F[_]: FromFuture: Monad](db: FutureDB){
   *            def query(lines: Int): F[Unit] = FromFuture.defer[F](db.query(lines))
   *          }
   * }}} */
  def defer[F[_]]: FromFutureDeferPA[F] = new FromFutureDeferPA[F]()

  private[FromFuture] class FromFutureDeferPA[F[_]](val __ : Boolean = true) extends AnyVal {
    def apply[A](future: => Future[A])(implicit F: FromFuture[F]): F[A] = F(future)
  }

  def apply[F[_]](implicit F: FromFuture[F]): FromFuture[F] = F

  def summon[F[_]](implicit F: FromFuture[F]): FromFuture[F] = F


  implicit def lift[F[_] : Async](implicit executor: ExecutionContext): FromFuture[F] = {

    new FromFuture[F] {

      def apply[A](future: => Future[A]) = {
        for {
          future <- Sync[F].delay(future)
          result <- future.value.fold {
            Async[F].async[A] { callback =>
              future.onComplete { a =>
                callback(a.toEither)
              }
            }
          } {
            case Success(a) => Async[F].pure(a)
            case Failure(a) => Async[F].raiseError[A](a)
          }
        } yield result
      }
    }
  }


  def functionK[F[_] : FromFuture]: FunctionK[Future, F] = new FunctionK[Future, F] {

    def apply[A](fa: Future[A]) = FromFuture.summon[F].apply(fa)
  }


  implicit val futureFromFuture: FromFuture[Future] = new FromFuture[Future] {
    def apply[A](future: => Future[A]) = future
  }


  implicit class FromFutureOps[F[_]](val self: FromFuture[F]) extends AnyVal {

    def toFunctionK: FunctionK[Future, F] = functionK(self)
  }
}