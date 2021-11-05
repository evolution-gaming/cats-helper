import Dependencies._

inThisBuild(Seq(
  homepage := Some(new URL("http://github.com/evolution-gaming/cats-helper")),

  organization := "com.evolutiongaming",
  organizationName := "Evolution",
  organizationHomepage := Some(url("http://evolution.com")),

  startYear := Some(2019),
  licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT"))),

  crossScalaVersions := Seq("2.13.7", "2.12.15"),
  scalaVersion := crossScalaVersions.value.head,

  publishTo := Some(Resolver.evolutionReleases),

  addCompilerPlugin(cpKindProjector),
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
      machinist,
      `slf4j-api`,
      scalatest % Test,
      logback % Test,
    ),
  )
  .dependsOn(
    testkit % Test,
  )

lazy val testkit = project
  .settings(
    commonSettings,

    name := "cats-helper-testkit",

    libraryDependencies ++= Seq(
      Cats.effectLaws,
      scalatest % Optional,
    ),
  )
