ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaTelegramDnDBot",
    libraryDependencies ++= List(
      Dependencies.telegramiumCore,
      Dependencies.telegramiumHigh,
      Dependencies.catsEffect,
      Dependencies.sttpClient,
      Dependencies.tapirCore,
      Dependencies.logger,
      Dependencies.circleCore,
      Dependencies.circleGeneric,
      Dependencies.circleParser,
      Dependencies.circleSoft,
      Dependencies.httpClient,
      Dependencies.scalaTest
    ),
    assembly / assemblyJarName := "ScalaTelegramDnDBot.jar",
    assembly / test := {},
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _                             => MergeStrategy.first
    }
  )
