name := "swagger-codegen"

version := "1.2-SNAPSHOT"

organization := "com.wordnik"

libraryDependencies ++= Seq(
  "org.codehaus.jackson" % "jackson-jaxrs" % "1.9.5",
  "org.codehaus.jackson" % "jackson-xc" % "1.9.5",
  "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.5",
  "org.codehaus.jackson" % "jackson-core-asl" % "1.9.5",
  "commons-io" % "commons-io" % "2.4",
  "com.wordnik" % "swagger-core_2.9.1" % "1.1-SNAPSHOT" % "compile",
  "org.fusesource.scalate" % "scalate-wikitext" % "1.5.3",
  "org.fusesource.scalate" % "scalate-page" % "1.5.3",
  "junit" % "junit" % "4.8.1" % "test"
)