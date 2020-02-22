import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
  lazy val zioStream = "dev.zio" %% "zio-streams" % "1.0.0-RC17"
  lazy val zioTest = "dev.zio" %% "zio-test" % "1.0.0-RC17"
  lazy val zioInteropCats = "dev.zio" %% "zio-interop-cats" % "2.0.0.0-RC1"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.0.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "2.0.0"
  lazy val simulacrum = "org.typelevel" %% "simulacrum" % "1.0.0"
}
