lazy val root = (project in file(".")).
  settings(
    organization := "io.swagger",
    name := "swagger-java-client",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions ++= Seq("-Xlint:deprecation"),
    resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.5.0",
      "com.squareup.okhttp" % "okhttp" % "2.4.0",
      "com.google.code.gson" % "gson" % "2.3.1",
      "junit" % "junit" % "4.8.1" % "test"
    )
  )
