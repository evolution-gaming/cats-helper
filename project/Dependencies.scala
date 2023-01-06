import sbt._

object Dependencies {

  val scalatest   = "org.scalatest"     %% "scalatest"       % "3.2.14"
  val machinist   = "org.typelevel"     %% "machinist"       % "0.6.8"
  val `slf4j-api` = "org.slf4j"          % "slf4j-api"       % "2.0.5"

  object Logback {
    val classic = "ch.qos.logback" % "logback-classic" % "1.4.5"
  }

  val cpKindProjector = "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full

  object Cats {
    private val version = "2.9.0"
    val core   = "org.typelevel" %% "cats-core"   % version
    val kernel = "org.typelevel" %% "cats-kernel" % version
    val macros = "org.typelevel" %% "cats-macros" % version

    private val effectVersion = "2.5.5"
    val effect     = "org.typelevel" %% "cats-effect"      % effectVersion
    val effectLaws = "org.typelevel" %% "cats-effect-laws" % effectVersion
  }
}
