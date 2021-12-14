ThisBuild / name := "matasuka"

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0"

ThisBuild / libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.7"


lazy val root = (project in file("."))
    .dependsOn(macrox)
    .settings()

lazy val macrox = (project in file("macrox"))
    .settings()