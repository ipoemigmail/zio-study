import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "zio-study",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += catsCore,
    libraryDependencies += catsEffect,
    libraryDependencies += zioStream,
    libraryDependencies += zioInteropCats,
    libraryDependencies += simulacrum,
    scalacOptions += "-Ymacro-annotations"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
