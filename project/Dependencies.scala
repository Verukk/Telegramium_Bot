import sbt._

object Dependencies {
  val telegramiumCore = "io.github.apimorphism" %% "telegramium-core" % "9.77.0"
  val telegramiumHigh = "io.github.apimorphism" %% "telegramium-high" % "9.77.0"
  val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.4"
  val sttpClient = "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M22"
  val tapirCore = "com.softwaremill.sttp.tapir" %% "tapir-core" % "1.10.15"
  val logger = "ch.qos.logback" % "logback-classic" % "1.5.6"
  val scalaTest = "org.scalatest" %% "scalatest" % "3.2.19" % Test

  val circleCore = "io.circe" %% "circe-core" % "0.14.9"
  val circleGeneric = "io.circe" %% "circe-generic" % "0.14.9"
  val circleParser = "io.circe" %% "circe-parser" % "0.14.9"
  val circleSoft = "com.softwaremill.sttp.client4" %% "circe" % "4.0.0-M22"
  val httpClient = "com.softwaremill.sttp.client4" %% "cats" % "4.0.0-M22"

}
