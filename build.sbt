import Dependencies._

def crossSettings[T](scalaVersion: String, if3: Seq[T], if2: Seq[T]) = {
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((3, _)) => if3
    case Some((2, 12 | 13)) => if2
    case _ => Nil
  }
}

inThisBuild(Seq(
  homepage := Some(new URL("http://github.com/evolution-gaming/cats-helper")),

  organization := "com.evolutiongaming",
  organizationName := "Evolution",
  organizationHomepage := Some(url("http://evolution.com")),

  startYear := Some(2019),
  licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT"))),

  crossScalaVersions := Seq("2.13.12", "3.3.3", "2.12.18"),

  versionScheme := Some("semver-spec"),

  scalaVersion := crossScalaVersions.value.head,

  publishTo := Some(Resolver.evolutionReleases),

  autoAPIMappings := true,
))

// Settings that can't be defined on a higher level go here.
// Usually such settings have defaults defined by some plugin in its `projectSettings`.
lazy val commonSettings = Seq(
  releaseCrossBuild := true,
  scalacOptsFailOnWarn := Some(false),
)

lazy val root = project
  .in(file("."))
  .settings(
    commonSettings,
    name := "cats-helper",
    publish / skip := true,
    publishArtifact := false,
  )
  .aggregate(
    core,
    logback,
    testkit,
  )

lazy val core = project
  .settings(
    commonSettings,
    // formerly this was a top-level module and thus it retains the old name
    name := "cats-helper",
    libraryDependencies ++= Seq(
      Cats.core,
      Cats.kernel,
      Cats.effect,
      `slf4j-api`,
      Logback.classic % Test,
      scalatest % Test,
    ),
    libraryDependencies ++= crossSettings(
      scalaVersion.value,
      if3 = Nil,
      if2 = List(compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full))
    ),
    scalacOptions ++= crossSettings(
      scalaVersion.value,
      if3 = Seq("-Ykind-projector:underscores", "-language:implicitConversions"),
      if2 = List("-Xsource:3", "-P:kind-projector:underscore-placeholders")
    ),
  )
  .dependsOn(
    testkit % Test,
  )

lazy val logback = project
  .settings(
    commonSettings,
    name := "cats-helper-logback",
    libraryDependencies ++= Seq(
      Logback.classic,
      scalatest % Test,
    )
  )
  .dependsOn(
    core,
    testkit % Test,
  )

lazy val testkit = project
  .settings(
    commonSettings,
    name := "cats-helper-testkit",
    libraryDependencies ++= Seq(
      Cats.effectStd,
      Cats.effectTestkit,
      Cats.effectLaws,
      scalatest % Optional,
    ),
  )
