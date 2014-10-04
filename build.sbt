name := "teamo"

version := "0.1"

scalaVersion := "2.11.2"


libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "compile",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
  )

unmanagedSourceDirectories in Compile += baseDirectory.value / "src/fixture/scala"
