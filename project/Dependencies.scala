import sbt._

object Dependencies {

  val scalatest = "org.scalatest" %% "scalatest" % "3.2.16"
  val `slf4j-api` = "org.slf4j" % "slf4j-api" % "2.0.5"

  object Logback {
    val classic = "ch.qos.logback" % "logback-classic" % "1.5.7"
  }

  object Cats {
    private val version = "2.10.0"
    val core = "org.typelevel" %% "cats-core" % version
    val kernel = "org.typelevel" %% "cats-kernel" % version
    val macros = "org.typelevel" %% "cats-macros" % version

    private val effectVersion = "3.5.3"
    val effect = "org.typelevel" %% "cats-effect" % effectVersion
    val effectLaws = "org.typelevel" %% "cats-effect-laws" % effectVersion
    val effectTestkit = "org.typelevel" %% "cats-effect-testkit" % effectVersion
    val effectStd = "org.typelevel" %% "cats-effect-std" % effectVersion
  }
}
