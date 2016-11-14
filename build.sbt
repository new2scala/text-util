name := "text utilities"

lazy val json4sVersion = "3.4.2"

lazy val commonSettings = Seq(
  organization := "org.dele",
  version := "0.1.0",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.6",
    "org.json4s" %% "json4s-jackson" % json4sVersion,
    "joda-time" % "joda-time" % "2.9.4",
    "org.testng" % "testng" % "6.9.10"
  )
)

lazy val root = project.in(file(".")).settings(commonSettings: _*).aggregate(maen, lapa)

lazy val maen = project.settings(commonSettings: _*)
lazy val lapa = project.settings(commonSettings: _*)

