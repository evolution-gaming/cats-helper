package com.evolutiongaming.catshelper

/** Example usage:
  *    
  * {{{
  * class ExampleService[F[_]: Logging] extends StrictLogging {
  * 
  *   def doSomething: F[Unit] =
  *     logger.info("Doing something")
  * 
  * }
  * }}} 
  */
trait StrictLogging {

  protected val logger: Logger = Logger.forClass(getClass)
  
}
