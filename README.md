# Swagger Client Code-Generator

## Overview
This is a project to build the Swagger code-gen library which can be used to automatically
generate client libraries from a Swagger-compliant server.  You can find out more about both 
the spec and the framework at http://swagger.wordnik.com.  For more information about Wordnik's 
APIs, please visit http://developer.wordnik.com.  

### Prerequisites
You need the following installed and available in your $PATH:

<li>- Java 1.6 or greater (http://java.oracle.com)

<li>- Apache Maven 3.0 or greater

<li>- Scala 2.9.1  (http://www.scala-lang.org/downloads)

You also need to set an environment variable for SCALA_HOME:

```
export SCALA_HOME={PATH_TO_YOUR_SCALA_DEPLOYMENT}
```

### To build the codegen library

```
mvn package
```

This will create the swagger-codegen library in your target folder.  

### Generating client libraries
Each language has a scala class in src/main/scala.  For instance ScalaCodegen can be run as a script as follows:

```
./bin/runscala.sh src/main/scala/ScalaCodegen ${API_PATH} ${API_KEY}
```

You can see an example for `scala-wordnik-api.sh` 
