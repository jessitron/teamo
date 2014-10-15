name := "teamo"

version := "0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.6" % "compile",
  "com.typesafe.akka" %% "akka-agent" % "2.3.6" % "compile",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "compile",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
  )

unmanagedSourceDirectories in Compile += baseDirectory.value / "src/fixture/scala"
