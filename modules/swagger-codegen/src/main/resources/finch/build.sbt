scalariformSettings

organization := "io.swagger"

name := "finch-sample"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "TM" at "http://maven.twttr.com"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"

Defaults.itSettings

scalacOptions += "-language:postfixOps"

lazy val `it-config-sbt-project` = project.in(file(".")).configs(IntegrationTest)

libraryDependencies ++= Seq(
  "com.github.finagle"      %% "finch-core"                     % "0.9.3",
  "com.github.finagle"      %% "finch-argonaut"                 % "0.9.3",
  "io.argonaut"             %% "argonaut"                       % "6.1",
  "com.github.finagle"      %% "finch-test"                     % "0.9.3"      % "test",
  "org.scalacheck"          %% "scalacheck"                     % "1.12.5"     % "test",
  "org.scalatest"           %% "scalatest"                      % "2.2.5"      % "test"
)

assemblyMergeStrategy in assembly := {
  case "application.conf"  => MergeStrategy.concat
  case "about.html"     => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
