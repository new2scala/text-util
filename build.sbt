name := "text utilities"

lazy val commonSettings = Seq(
  organization := "org.dele",
  version := "0.1.0",
  scalaVersion := "2.11.8"
)

lazy val root = project.in(file(".")).settings(commonSettings: _*).aggregate(maen, lapa)

lazy val maen = project.settings(commonSettings: _*)
lazy val lapa = project.settings(commonSettings: _*)

