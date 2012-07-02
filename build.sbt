name := "swagger-codegen"

version := "1.2-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.wordnik" % "swagger-core_2.9.1" % "1.02-SNAPSHOT" % "compile",
  "org.fusesource.scalate" % "scalate-wikitext" % "1.5.3",
  "org.fusesource.scalate" % "scalate-page" % "1.5.3",
  "junit" % "junit" % "4.8.1" % "test"
)