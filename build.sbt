import Dependencies._

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.wsf_lp"
ThisBuild / organizationName := "wsf"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

lazy val root = (project in file("."))
  .settings(
    name := "brainfuck",
    libraryDependencies ++= Seq(munit % Test, "com.github.scopt" %% "scopt" % "4.1.0") 
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
