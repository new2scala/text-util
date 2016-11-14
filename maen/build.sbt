name := "text match engine"

val json4sVersion = "3.4.2"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6",
  "org.json4s" %% "json4s-jackson" % json4sVersion,
  "joda-time" % "joda-time" % "2.9.4",
  "org.testng" % "testng" % "6.9.10"
)