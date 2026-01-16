package com.evolutiongaming.catshelper

import scala.reflect.ClassTag

/** Named [[Logger]] instance.
  * 
  * Could be used to log the messages safely if [[Logging]] is available or
  * unsafely by calling [[Logger#unsafe]] method instead (in this case one
  * might want to wrap the call into `Sync[F].delay` to keep the referential
  * transparency intact).
  */
class Logger(slf4j: org.slf4j.Logger) {
  
  def unsafe: org.slf4j.Logger = slf4j

  def debug[F[_]: Logging](message: String): F[Unit] =
    Logging[F].debug(this, message)

  def info[F[_]: Logging](message: String): F[Unit] =
    Logging[F].info(this, message)

  def warn[F[_]: Logging](message: String): F[Unit] =
    Logging[F].warn(this, message)

  def error[F[_]: Logging](message: String): F[Unit] =
    Logging[F].error(this, message)

}

object Logger {

  def forClass[C](implicit tag: ClassTag[C]): Logger =
    forClass(tag.runtimeClass)

  def forClass(clazz: Class[?]): Logger =
    new Logger(org.slf4j.LoggerFactory.getLogger(clazz))
    
}