scalaVersion := "2.13.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += guice

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
