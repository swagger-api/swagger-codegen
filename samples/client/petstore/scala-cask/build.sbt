name := "caskgen"
version := "0.0.1-SNAPSHOT"
scalaVersion := "3.2.0"
scalafmtOnCompile := true
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "cask" % "0.8.3" ,
  "com.lihaoyi" %% "upickle" % "1.6.0",
  "com.softwaremill.sttp.client3" %% "core" % "3.8.3"
)
