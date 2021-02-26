import Dependencies._

inThisBuild(Seq(
  homepage := Some(new URL("http://github.com/evolution-gaming/cats-helper")),

  organization := "com.evolutiongaming",
  organizationName := "Evolution Gaming",
  organizationHomepage := Some(url("http://evolutiongaming.com")),
  bintrayOrganization := Some("evolutiongaming"),

  startYear := Some(2019),
  licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT"))),

  crossScalaVersions := Seq("2.13.5", "2.12.12"),
  scalaVersion := crossScalaVersions.value.head,

  resolvers += Resolver.bintrayRepo("evolutiongaming", "maven"),

  addCompilerPlugin(cpKindProjector),
))

// Settings that can't be defined on a higher level go here.
// Usually such settings have defaults defined by some plugin in its `projectSettings`.
lazy val commonSettings = Seq(
  releaseCrossBuild := true,
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
