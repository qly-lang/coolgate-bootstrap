name := "coolgate-bootstrap"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies ++=
  Seq("io.bullet" %% "borer-core" % "1.7.1", "io.bullet" %% "borer-derivation" % "1.7.1")
libraryDependencies += "org.rogach" %% "scallop" % "4.0.2"
