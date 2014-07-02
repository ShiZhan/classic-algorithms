name := "ClassicAlgorithms"

version := "1.0"

scalaVersion := Option(System.getProperty("scala.version")).getOrElse("2.10.4")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-actors" % "2.10.4"
)
