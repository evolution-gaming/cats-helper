package com.evolutiongaming

import cats.effect.MonadCancel
import cats.{ApplicativeError, MonadError}

package object catshelper {

  type ApplicativeThrowable[F[_]] = ApplicativeError[F, Throwable]

  object ApplicativeThrowable {

    def apply[F[_]](implicit F: ApplicativeThrowable[F]): ApplicativeThrowable[F] = F

    def summon[F[_]](implicit F: ApplicativeThrowable[F]): ApplicativeThrowable[F] = F
  }


  type MonadThrowable[F[_]] = MonadError[F, Throwable]

  object MonadThrowable {

    def apply[F[_]](implicit F: MonadThrowable[F]): MonadThrowable[F] = F

    def summon[F[_]](implicit F: MonadThrowable[F]): MonadThrowable[F] = F
  }


  type BracketThrowable[F[_]] = MonadCancel[F, Throwable]


  object BracketThrowable {

    def apply[F[_]](implicit F: BracketThrowable[F]): BracketThrowable[F] = F

    def summon[F[_]](implicit F: BracketThrowable[F]): BracketThrowable[F] = F
  }
}
