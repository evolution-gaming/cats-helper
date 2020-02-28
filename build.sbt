import Dependencies._

inThisBuild(Seq(
  homepage := Some(new URL("http://github.com/evolution-gaming/cats-helper")),

  organization := "com.evolutiongaming",
  organizationName := "Evolution Gaming",
  organizationHomepage := Some(url("http://evolutiongaming.com")),
  bintrayOrganization := Some("evolutiongaming"),

  startYear := Some(2019),
  licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT"))),

  crossScalaVersions := Seq("2.13.1", "2.12.10"),
  scalaVersion := crossScalaVersions.value.head,
  releaseCrossBuild := true,

  resolvers += Resolver.bintrayRepo("evolutiongaming", "maven"),

  addCompilerPlugin(cpKindProjector),
))

lazy val root = project
  .in(file("."))
  .settings(
    publishArtifact := false,
  )
  .aggregate(
    core,
    testkit,
  )

lazy val core = project
  .settings(
    // formerly this was a top-level module and thus it retains the old name
    name := "cats-helper",

    libraryDependencies ++= Seq(
      Cats.core,
      Cats.kernel,
      Cats.macros,
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
    name := "cats-helper-testkit",

    libraryDependencies ++= Seq(
      Cats.effectLaws,
      scalatest,
    ),
  )
