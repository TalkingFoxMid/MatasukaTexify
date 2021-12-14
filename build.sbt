ThisBuild / name := "matasuka"

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0"

ThisBuild / libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.7"

ThisBuild / libraryDependencies += "org.typelevel" %% "mouse" % "1.0.8"

ThisBuild / libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M6"

scalacOptions += "-language:implicitConversions"
scalacOptions += "-Xfatal-warnings"
scalacOptions += "-Wconf:cat=other-match-analysis:error"
lazy val root = (project in file("."))
    .dependsOn(macrox)
    .settings()

lazy val macrox = (project in file("macrox"))
    .settings()

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
