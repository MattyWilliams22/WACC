ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "WACC_04",
    libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.5.1"
  )
