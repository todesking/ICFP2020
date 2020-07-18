scalaVersion := "2.13.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += guice

lazy val root = (project in file(".")).enablePlugins(PlayScala)
