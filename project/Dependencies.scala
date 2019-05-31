import sbt._

object Dependencies {

  val scalatest   = "org.scalatest"     %% "scalatest" % "3.0.7"
  val machinist   = "org.typelevel"     %% "machinist" % "0.6.8"
  val `slf4j-api` = "org.slf4j"          % "slf4j-api" % "1.7.26"
  val `cats-par`  = "io.chrisdavenport" %% "cats-par"  % "0.2.1"

  object Cats {
    private val version = "1.6.0"
    val core   = "org.typelevel" %% "cats-core"   % version
    val kernel = "org.typelevel" %% "cats-kernel" % version
    val macros = "org.typelevel" %% "cats-macros" % version
    val effect = "org.typelevel" %% "cats-effect" % "1.3.1"
  }
}