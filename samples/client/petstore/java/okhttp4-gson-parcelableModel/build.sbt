lazy val root = (project in file(".")).
  settings(
    organization := "io.swagger",
    name := "swagger-petstore-okhttp4-gson",
    version := "1.0.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-feature"),
    javacOptions in compile ++= Seq("-Xlint:deprecation"),
    publishArtifact in (Compile, packageDoc) := false,
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "io.swagger" % "swagger-annotations" % "1.6.9",
      "com.squareup.okhttp" % "okhttp" % "4.10.0",
      "com.squareup.okhttp" % "logging-interceptor" % "4.10.0",
      "com.google.code.gson" % "gson" % "2.10.1",
      "org.threeten" % "threetenbp" % "1.6.5" % "compile",
      "io.gsonfire" % "gson-fire" % "1.8.5" % "compile",
      "junit" % "junit" % "4.13.2" % "test",
      "com.novocode" % "junit-interface" % "0.10" % "test"
    )
  )
