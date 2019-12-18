import sbt._

object Dependencies {

  val scalatest   = "org.scalatest"     %% "scalatest" % "3.1.0"
  val machinist   = "org.typelevel"     %% "machinist" % "0.6.8"
  val `slf4j-api` = "org.slf4j"          % "slf4j-api" % "1.7.29"

  object Cats {
    private val version = "2.0.0"
    val core   = "org.typelevel" %% "cats-core"   % version
    val kernel = "org.typelevel" %% "cats-kernel" % version
    val macros = "org.typelevel" %% "cats-macros" % version
    val effect = "org.typelevel" %% "cats-effect" % "2.1.0"
  }
}