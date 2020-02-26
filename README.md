# <img src="https://raw.githubusercontent.com/swagger-api/swagger.io/wordpress/images/assets/SWC-logo-clr.png" height="80">

[![Build Status](https://img.shields.io/jenkins/build.svg?jobUrl=https://jenkins.swagger.io/job/oss-swagger-codegen-3)](https://jenkins.swagger.io/view/OSS%20-%20Java/job/oss-swagger-codegen-3)

- 3.0.19-SNAPSHOT:  [![Build Status](https://img.shields.io/travis/swagger-api/swagger-codegen/3.0.0.svg?label=Petstore%20Integration%20Test)](https://travis-ci.org/swagger-api/swagger-codegen)
[![Java Test](https://img.shields.io/jenkins/build.svg?jobUrl=https://jenkins.swagger.io/job/oss-swagger-codegen-3)](https://jenkins.swagger.io/view/OSS%20-%20Java/job/oss-swagger-codegen-3)
- Master (2.4.13-SNAPSHOT): [![Build Status](https://img.shields.io/travis/swagger-api/swagger-codegen/master.svg?label=Petstore%20Integration%20Test)](https://travis-ci.org/swagger-api/swagger-codegen)
[![Java Test](https://img.shields.io/jenkins/build.svg?jobUrl=https://jenkins.swagger.io/job/oss-swagger-codegen-master)](https://jenkins.swagger.io/view/OSS%20-%20Java/job/oss-swagger-codegen-master)
[![Windows Test](https://ci.appveyor.com/api/projects/status/github/swagger-api/swagger-codegen?branch=master&svg=true&passingText=Windows%20Test%20-%20OK&failingText=Windows%20Test%20-%20Fails)](https://ci.appveyor.com/project/swaggerhub-bot/swagger-codegen)

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.swagger.codegen.v3/swagger-codegen-project/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/io.swagger.codegen.v3/swagger-codegen-project)

:warning: If the OpenAPI/Swagger spec is obtained from an untrusted source, please make sure you've reviewed the spec before using Swagger Codegen to generate the API client, server stub or documentation as [code injection](https://en.wikipedia.org/wiki/Code_injection) may occur :warning:

## Versioning

**NOTE:** version 2.X (`io.swagger`) and 3.X (`io.swagger.codegen.v3`) have **different** group ids.

2.X and 3.X version lines of Swagger Codegen are available; 2.X ([`master` branch](https://github.com/swagger-api/swagger-codegen/tree/master)) supports Swagger/OpenAPI version 2,
while 3.X ([`3.0.0` branch](https://github.com/swagger-api/swagger-codegen/tree/3.0.0)) supports OpenAPI version 3 (and version 2 via spec conversion to version 3).
[Online generator of version 3.X](https://github.com/swagger-api/swagger-codegen/tree/3.0.0#online-generators) supports both generation from Swagger/OpenAPI version 2 (by using engine + generators of 2.X) and version 3 specifications.


**NOTE:** this document refers to version 3.X, check [here](https://github.com/swagger-api/swagger-codegen/tree/master) for 2.X.

### Swagger Codegen 3.X ([`3.0.0` branch](https://github.com/swagger-api/swagger-codegen/tree/3.0.0))

Swagger Codegen 2.X supports OpenAPI version 3 (and version 2 via spec conversion to version 3)
[Online generator of version 3.X](https://github.com/swagger-api/swagger-codegen/tree/3.0.0#online-generators) supports both generation from Swagger/OpenAPI version 2 (by using engine + generators of 2.X) and version 3 specifications.

group id: `io.swagger.codegen.v3`
maven central: https://mvnrepository.com/artifact/io.swagger.codegen.v3

dependency example:

```
<dependency>
    <groupId>io.swagger.codegen.v3</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
    <version>3.0.18</version>
</dependency>
```

### Swagger Codegen 2.X ([`master` branch](https://github.com/swagger-api/swagger-codegen/tree/master))

Swagger Codegen 2.X supports Swagger/OpenAPI version 2.

group id: `io.swagger`
maven central (maven plugin): https://mvnrepository.com/artifact/io.swagger/swagger-codegen-maven-plugin

dependency example:

```
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-codegen-maven-plugin</artifactId>
    <version>2.4.12</version>
</dependency>
```

## Overview
This is the Swagger Codegen project, which allows generation of API client libraries (SDK generation), server stubs and documentation automatically given an [OpenAPI Definition](https://github.com/OAI/OpenAPI-Specification). Currently, the following languages/frameworks are supported:

- **API clients**: **Java** (Jersey1.x, Jersey2.x, OkHttp, Retrofit1.x, Retrofit2.x, Feign, RestTemplate, RESTEasy, Vertx, Google API Client Library for Java)
- **Server stubs**: **Java** (Inflector)
- **API documentation generators**: **HTML**, **Confluence Wiki**
- **Configuration files**: [**Apache2**](https://httpd.apache.org/)
- **Others**: **JMeter**

Check out [OpenAPI Specification](https://github.com/OAI/OpenAPI-Specification) for additional information about the OpenAPI project.

# Table of contents

  - [Swagger Code Generator](#swagger-code-generator)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - Installation
    - [Compatibility](#compatibility)
    - [Prerequisites](#prerequisites)
      - [OS X Users](#os-x-users)
    - [Building](#building)
    - [Docker](#docker)
      - [Development in Docker](#development-in-docker)
      - [Run docker in Vagrant](#run-docker-in-vagrant)
      - [Public Docker image](#public-pre-built-docker-images)
      - [Swagger Generator Docker Image](#swagger-generator--docker-image)
      
    - [Homebrew](#homebrew)
  - [Getting Started](#getting-started)
  - Generators
    - [To generate a sample client library](#to-generate-a-sample-client-library)
    - [Generating libraries from your server](#generating-libraries-from-your-server)
    - [Modifying the client library format](#modifying-the-client-library-format)
    - [Making your own codegen modules](#making-your-own-codegen-modules)
    - [Generating a client from local files](#generating-a-client-from-local-files)
    - [Customizing the generator](#customizing-the-generator)
    - [Validating your OpenAPI Spec](#validating-your-openapi-spec)
    - [Generating dynamic html api documentation](#generating-dynamic-html-api-documentation)
    - [To build a server stub](#to-build-a-server-stub)
    - [To build the codegen library](#to-build-the-codegen-library)
  - [Workflow Integration](#workflow-integration)
    - [Maven Integration](#maven-integration)
    - [Gradle Integration](#gradle-integration)
  - [Github Integration](#github-integration)
  - [Online Generators](#online-generators)
  - [Guidelines for Contribution](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md#guidelines-for-contributing)
  - [License](#license)


## Compatibility
The OpenAPI Specification has undergone 3 revisions since initial creation in 2010.. The Swagger Codegen project has the following compatibilities with the OpenAPI Specification:

Swagger Codegen Version    | Release Date | OpenAPI Spec compatibility | Notes
-------------------------- | ------------ | -------------------------- | -----
3.0.19-SNAPSHOT (current 3.0.0, upcoming minor release) [SNAPSHOT](https://oss.sonatype.org/content/repositories/snapshots/io/swagger/codegen/v3/swagger-codegen-cli/3.0.19-SNAPSHOT/)| TBD | 1.0, 1.1, 1.2, 2.0, 3.0 | Minor release
[3.0.18](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.18) (**current stable**) | 2020-02-26   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.18](https://github.com/swagger-api/swagger-codegen/tree/v3.0.18)
[3.0.17](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.17) (**current stable**) | 2020-02-23   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.17](https://github.com/swagger-api/swagger-codegen/tree/v3.0.17)
[3.0.16](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.16) (**current stable**) | 2020-01-15   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.16](https://github.com/swagger-api/swagger-codegen/tree/v3.0.16)
[3.0.15](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.15) (**current stable**) | 2020-01-03   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.15](https://github.com/swagger-api/swagger-codegen/tree/v3.0.15)
[3.0.14](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.14) | 2019-11-16   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.14](https://github.com/swagger-api/swagger-codegen/tree/v3.0.14)
[3.0.13](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.13) | 2019-10-16   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.13](https://github.com/swagger-api/swagger-codegen/tree/v3.0.13)
[3.0.12](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.12) | 2019-10-14   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.12](https://github.com/swagger-api/swagger-codegen/tree/v3.0.12)
[3.0.11](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.11) | 2019-08-24   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.11](https://github.com/swagger-api/swagger-codegen/tree/v3.0.11)
[3.0.10](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.10) | 2019-07-11   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.10](https://github.com/swagger-api/swagger-codegen/tree/v3.0.10)
[3.0.9](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.9) | 2019-06-28   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.9](https://github.com/swagger-api/swagger-codegen/tree/v3.0.9)
[3.0.8](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.8) | 2019-04-25   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.8](https://github.com/swagger-api/swagger-codegen/tree/v3.0.8)
[3.0.7](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.7) | 2019-03-26   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.7](https://github.com/swagger-api/swagger-codegen/tree/v3.0.7)
[3.0.5](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.5) | 2019-02-18   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.5](https://github.com/swagger-api/swagger-codegen/tree/v3.0.5)
[3.0.4](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.4) | 2019-01-16   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.4](https://github.com/swagger-api/swagger-codegen/tree/v3.0.4)
[3.0.3](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.3) | 2018-11-30   | 1.0, 1.1, 1.2, 2.0, 3.0   | [tag v3.0.3](https://github.com/swagger-api/swagger-codegen/tree/v3.0.3)
[3.0.2](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.2)| 2018-10-19 | 1.0, 1.1, 1.2, 2.0, 3.0 | Minor release
[3.0.1](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.1)| 2018-10-05 | 1.0, 1.1, 1.2, 2.0, 3.0 | Major release with breaking changes
[3.0.0](https://github.com/swagger-api/swagger-codegen/releases/tag/v3.0.0)| 2018-09-06 | 1.0, 1.1, 1.2, 2.0, 3.0 | Major release with breaking changes
2.4.13-SNAPSHOT (current master, upcoming minor release) [SNAPSHOT](https://oss.sonatype.org/content/repositories/snapshots/io/swagger/swagger-codegen-cli/2.4.13-SNAPSHOT/)| TBD   | 1.0, 1.1, 1.2, 2.0   | Minor release
[2.4.12](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.12) (**current stable**) | 2020-01-15   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.12](https://github.com/swagger-api/swagger-codegen/tree/v2.4.12)
[2.4.11](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.11) (**current stable**) | 2020-01-03   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.11](https://github.com/swagger-api/swagger-codegen/tree/v2.4.11)
[2.4.10](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.10) | 2019-11-16   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.10](https://github.com/swagger-api/swagger-codegen/tree/v2.4.10)
[2.4.9](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.9) | 2019-10-14   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.9](https://github.com/swagger-api/swagger-codegen/tree/v2.4.9)
[2.4.8](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.8) | 2019-08-24   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.8](https://github.com/swagger-api/swagger-codegen/tree/v2.4.8)
[2.4.7](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.7) | 2019-07-11   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.7](https://github.com/swagger-api/swagger-codegen/tree/v2.4.7)
[2.4.6](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.6) | 2019-06-28   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.6](https://github.com/swagger-api/swagger-codegen/tree/v2.4.6)
[2.4.5](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.5)  | 2019-04-25   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.5](https://github.com/swagger-api/swagger-codegen/tree/v2.4.5)
[2.4.4](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.4) | 2019-03-26   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.4](https://github.com/swagger-api/swagger-codegen/tree/v2.4.4)
[2.4.2](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.2) | 2019-02-18   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.2](https://github.com/swagger-api/swagger-codegen/tree/v2.4.2)
[2.4.1](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.1) | 2019-01-16   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.1](https://github.com/swagger-api/swagger-codegen/tree/v2.4.1)
[2.4.0](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.4.0) | 2018-11-30   | 1.0, 1.1, 1.2, 2.0   | [tag v2.4.0](https://github.com/swagger-api/swagger-codegen/tree/v2.4.0)
[2.3.1](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.3.1) | 2018-01-17   | 1.0, 1.1, 1.2, 2.0   | [tag v2.3.1](https://github.com/swagger-api/swagger-codegen/tree/v2.3.1)
[2.3.0](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.3.0) | 2017-12-21   | 1.0, 1.1, 1.2, 2.0   | [tag v2.3.0](https://github.com/swagger-api/swagger-codegen/tree/v2.3.0)
[2.2.3](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.2.3) | 2017-07-15   | 1.0, 1.1, 1.2, 2.0   | [tag v2.2.3](https://github.com/swagger-api/swagger-codegen/tree/v2.2.3)
[2.2.2](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.2.2) | 2017-03-01   | 1.0, 1.1, 1.2, 2.0   | [tag v2.2.2](https://github.com/swagger-api/swagger-codegen/tree/v2.2.2)
[2.2.1](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.2.1) | 2016-08-07   | 1.0, 1.1, 1.2, 2.0   | [tag v2.2.1](https://github.com/swagger-api/swagger-codegen/tree/v2.2.1)
[2.1.6](https://github.com/swagger-api/swagger-codegen/releases/tag/v2.1.6) | 2016-04-06   | 1.0, 1.1, 1.2, 2.0   | [tag v2.1.6](https://github.com/swagger-api/swagger-codegen/tree/v2.1.6)
2.0.17                     | 2014-08-22   | 1.1, 1.2             | [tag v2.0.17](https://github.com/swagger-api/swagger-codegen/tree/2.0.17)
1.0.4                      | 2012-04-12   | 1.0, 1.1             | [tag v1.0.4](https://github.com/swagger-api/swagger-codegen/tree/swagger-codegen_2.9.1-1.1)


### Prerequisites
If you're looking for the latest stable version, you can grab it directly from Maven.org (Java 8 runtime at a minimum):

```sh
wget https://repo1.maven.org/maven2/io/swagger/codegen/v3/swagger-codegen-cli/3.0.18/swagger-codegen-cli-3.0.18.jar -O swagger-codegen-cli.jar

java -jar swagger-codegen-cli.jar --help
```

For Windows users, you will need to install [wget](http://gnuwin32.sourceforge.net/packages/wget.htm) or you can use Invoke-WebRequest in PowerShell (3.0+), e.g. `Invoke-WebRequest -OutFile swagger-codegen-cli.jar https://repo1.maven.org/maven2/io/swagger/codegen/v3/swagger-codegen-cli/3.0.18/swagger-codegen-cli-3.0.18.jar`

On a mac, it's even easier with `brew`:
```sh
brew install swagger-codegen
```

To build from source, you need the following installed and available in your $PATH:

* [Java 8](http://java.oracle.com)

* [Apache maven 3.3.3 or greater](http://maven.apache.org/)

#### OS X Users

Don't forget to install Java 8. 

Export JAVA_HOME in order to use the supported Java version:
```sh
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH=${JAVA_HOME}/bin:$PATH
```

### Building

After cloning the project, you can build it from source with this command:
```sh
mvn clean package
```

### Homebrew

To install, run `brew install swagger-codegen`

Here is an example usage:
```sh
swagger-codegen generate -i http://petstore.swagger.io/v2/swagger.json -l ruby -o /tmp/test/
```

### Docker

#### Development in docker

You can use `run-in-docker.sh` to do all development. This script maps your local repository to `/gen`
in the docker container. It also maps `~/.m2/repository` to the appropriate container location.

To execute `mvn package`:

```sh
git clone https://github.com/swagger-api/swagger-codegen
cd swagger-codegen
./run-in-docker.sh mvn package
```

Build artifacts are now accessible in your working directory.

Once built, `run-in-docker.sh` will act as an executable for swagger-codegen-cli. To generate code, you'll need to output to a directory under `/gen` (e.g. `/gen/out`). For example:

```sh
./run-in-docker.sh help # Executes 'help' command for swagger-codegen-cli
./run-in-docker.sh langs # Executes 'langs' command for swagger-codegen-cli
./run-in-docker.sh /gen/bin/go-petstore.sh  # Builds the Go client
./run-in-docker.sh generate -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml \
    -l go -o /gen/out/go-petstore -DpackageName=petstore # generates go client, outputs locally to ./out/go-petstore
```

#### Run Docker in Vagrant
Prerequisite: install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads).
 ```sh
git clone http://github.com/swagger-api/swagger-codegen.git
cd swagger-codegen
vagrant up
vagrant ssh
cd /vagrant
./run-in-docker.sh mvn package
 ```

#### Public Pre-built Docker images

 - https://hub.docker.com/r/swaggerapi/swagger-generator-v3/ (official web service)
 - https://hub.docker.com/r/swaggerapi/swagger-generator-v3-minimal/ (official minimal web service)
 - https://hub.docker.com/r/swaggerapi/swagger-codegen-cli-v3/ (official CLI)


##### Swagger Generator  Docker Image

See also [online generators](#online-generators)
The Swagger Generator image provides a ready to use web application (swagger-generator) providing code generation services.

Image accepts the following env variables:

- `JAVA_MEM` e.g. `1024m`
- `HTTP_PORT` e.g. `8080`
- `HIDDEN_OPTIONS_PATH` (alternative to `HIDDEN_OPTIONS`): useful if attaching a volume containing a `hiddenOptions.yaml` file definining which languages to hide. e.g. `/data/hiddenOptions.yaml`
- `HIDDEN_OPTIONS` (alternative to `HIDDEN_OPTIONS_PATH`): allows to pass hidden options as an env variable, in the format `{category}:{language},{language},{language}|{category}:{language},{language},{language}`
e.g. `servers:foo,bar|clientsV3:wtf,isthis` where category can be `clients`, `servers`, `clientsV3`, `serversV3`

An example of running the container:

`docker pull swaggerapi/swagger-generator-v3`

`docker run -e "HIDDEN_OPTIONS=servers:foo,bar|clientsV3:fgf,sdsd" -e "JAVA_MEM=1024m" -e "HTTP_PORT=80" -p 80:80 --name swagger-generator-v3 swaggerapi/swagger-generator-v3`

or

`docker run -e "HIDDEN_OPTIONS_PATH=/hiddenOptions.yaml" -e "JAVA_MEM=1024m" -e "HTTP_PORT=80" -p 80:80 --name swagger-generator-v3 swaggerapi/swagger-generator-v3`

This docker image supports custom generators by dropping the generator jar into `/jetty_home/lib/ext` directory (typically via a docker volume); e.g having on host `/my/custom/coolgenerator.jar` and `/my/custom/weirdgenerator.jar`  the following would have them added to generator service generators:

`docker run -e "HIDDEN_OPTIONS_PATH=/hiddenOptions.yaml" -e "JAVA_MEM=1024m" -e "HTTP_PORT=80" -p 80:80 --name swagger-generator-v3 -v /my/custom:/jetty_home/lib/shared swaggerapi/swagger-generator-v3`

##### Swagger Generator "Minimal" Docker Image

See also [online generators](#online-generators)

The Swagger Generator "Minimal" image can act as a self-hosted web application and API for generating code.

This container can be  incorporated into a CI pipeline, and requires some docker orchestration to access generated code.

Example usage:

```sh
# Start container and save the container id
CID=$(docker run -d swaggerapi/swagger-generator-v3-minimal)
# allow for startup
sleep 5
# Get the IP of the running container
GEN_IP=$(docker inspect --format '{{.NetworkSettings.IPAddress}}'  $CID)
# Execute an HTTP request and store the download link
curl -X POST \
           http://localhost:8080/api/generate \
           -H 'content-type: application/json' \
           -d '{
           "specURL" : "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v3.0/petstore.yaml",
           "lang" : "jaxrs-jersey",
           "type" : "SERVER",
           "codegenVersion" : "V3"
         }' > result.zip
# Shutdown the swagger generator image
docker stop $CID && docker rm $CID
```

In the example above, `result.zip` will contain the generated client.

##### Swagger Codegen CLI Docker Image

The Swagger Codegen image acts as a standalone executable. It can be used as an alternative to installing via homebrew, or for developers who are unable to install Java or upgrade the installed version.

To generate code with this image, you'll need to mount a local location as a volume.

Example:

```sh
docker run --rm -v ${PWD}:/local swaggerapi/swagger-codegen-cli-v3 generate \
    -i http://petstore.swagger.io/v2/swagger.json \
    -l go \
    -o /local/out/go
```

The generated code will be located under `./out/go` in the current directory.

## Getting Started

To generate a PHP client for http://petstore.swagger.io/v2/swagger.json, please run the following
```sh
git clone https://github.com/swagger-api/swagger-codegen
cd swagger-codegen
git checkout 3.0.0
mvn clean package
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
   -i http://petstore.swagger.io/v2/swagger.json \
   -l php \
   -o /var/tmp/php_api_client
```
(if you're on Windows, replace the last command with `java -jar modules\swagger-codegen-cli\target\swagger-codegen-cli.jar generate -i http://petstore.swagger.io/v2/swagger.json -l php -o c:\temp\php_api_client`)

You can also download the JAR (latest release) directly from [maven.org](https://repo1.maven.org/maven2/io/swagger/swagger-codegen-cli/2.3.0/swagger-codegen-cli-2.3.0.jar)

To get a list of **general** options available, please run `java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar help generate`

To get a list of PHP specified options (which can be passed to the generator with a config file via the `-c` option), please run `java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar config-help -l php`

## Generators

### To generate a sample client library
You can build a client against the swagger sample [petstore](http://petstore.swagger.io) API as follows:

```sh
./bin/java-petstore.sh
```

(On Windows, run `.\bin\windows\java-petstore.bat` instead)

This will run the generator with this command:

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java
```

with a number of options. You can get the options with the `help generate` command (below only shows partal results):

```
NAME
        swagger-codegen-cli generate - Generate code with chosen lang

SYNOPSIS
        swagger-codegen-cli generate
                [(-a <authorization> | --auth <authorization>)]
                [--additional-properties <additional properties>...]
                [--api-package <api package>] [--artifact-id <artifact id>]
                [--artifact-version <artifact version>]
                [(-c <configuration file> | --config <configuration file>)]
                [-D <system properties>...] [--git-repo-id <git repo id>]
                [--git-user-id <git user id>] [--group-id <group id>]
                [--http-user-agent <http user agent>]
                (-i <spec file> | --input-spec <spec file>)
                [--ignore-file-override <ignore file override location>]
                [--import-mappings <import mappings>...]
                [--instantiation-types <instantiation types>...]
                [--invoker-package <invoker package>]
                (-l <language> | --lang <language>)
                [--language-specific-primitives <language specific primitives>...]
                [--library <library>] [--model-name-prefix <model name prefix>]
                [--model-name-suffix <model name suffix>]
                [--model-package <model package>]
                [(-o <output directory> | --output <output directory>)]
                [--release-note <release note>] [--remove-operation-id-prefix]
                [--reserved-words-mappings <reserved word mappings>...]
                [(-s | --skip-overwrite)]
                [(-t <template directory> | --template-dir <template directory>)]
                [--type-mappings <type mappings>...] [(-v | --verbose)]

OPTIONS
        -a <authorization>, --auth <authorization>
            adds authorization headers when fetching the swagger definitions
            remotely. Pass in a URL-encoded string of name:header with a comma
            separating multiple values

...... (results omitted)

        -v, --verbose
            verbose mode

```

You can then compile and run the client, as well as unit tests against it:

```sh
cd samples/client/petstore/java
mvn package
```

Other languages have petstore samples, too:
```sh
./bin/android-petstore.sh
./bin/java-petstore.sh
./bin/objc-petstore.sh
```

### Generating libraries from your server
It's just as easy--just use the `-i` flag to point to either a server or file.

### Modifying the client library format
Don't like the default swagger client syntax?  Want a different language supported?  No problem!  Swagger Codegen processes mustache templates with the [jmustache](https://github.com/samskivert/jmustache) engine.  You can modify our templates or make your own.

You can look at `modules/swagger-codegen/src/main/resources/${your-language}` for examples.  To make your own templates, create your own files and use the `-t` flag to specify your template folder.  It actually is that easy.

### Making your own codegen modules
If you're starting a project with a new language and don't see what you need, Swagger Codegen can help you create a project to generate your own libraries:

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar meta \
  -o output/myLibrary -n myClientCodegen -p com.my.company.codegen
```

This will write, in the folder `output/myLibrary`, all the files you need to get started, including a `README.md. Once modified and compiled, you can load your library with the codegen and generate clients with your own, custom-rolled logic.

You would then compile your library in the `output/myLibrary` folder with `mvn package` and execute the codegen like such:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.v3.cli.SwaggerCodegen
```
For Windows users, you will need to use `;` instead of `:` in the classpath, e.g.
```
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar;modules/swagger-codegen-cli/target/swagger-codegen-cli.jar io.swagger.codegen.v3.cli.SwaggerCodegen
```

Note the `myClientCodegen` is an option now, and you can use the usual arguments for generating your library:

```sh
java -cp output/myLibrary/target/myClientCodegen-swagger-codegen-1.0.0.jar:modules/swagger-codegen-cli/target/swagger-codegen-cli.jar \
  io.swagger.codegen.v3.cli.SwaggerCodegen generate -l myClientCodegen\
  -i http://petstore.swagger.io/v2/swagger.json \
  -o myClient
```

### Generating a client from local files
If you don't want to call your server, you can save the OpenAPI Spec files into a directory and pass an argument
to the code generator like this:

```
-i ./modules/swagger-codegen/src/test/resources/2_0/petstore.json
```

Great for creating libraries on your ci server, from the [Swagger Editor](http://editor.swagger.io)... or while coding on an airplane.

### Selective generation
You may not want to generate *all* models in your project.  Likewise you may want just one or two apis to be written.  If that's the case, you can use system properties to control the output:

The default is generate *everything* supported by the specific library.  Once you enable a feature, it will restrict the contents generated:

```sh
# generate only models
java -Dmodels {opts}

# generate only apis
java -Dapis {opts}

# generate only supporting files
java -DsupportingFiles

# generate models and supporting files
java -Dmodels -DsupportingFiles
```

To control the specific files being generated, you can pass a CSV list of what you want:
```sh
# generate the User and Pet models only
-Dmodels=User,Pet

# generate the User model and the supportingFile `StringUtil.java`:
-Dmodels=User -DsupportingFiles=StringUtil.java
```

To control generation of docs and tests for api and models, pass false to the option. For api, these options are  `-DapiTests=false` and `-DapiDocs=false`. For models, `-DmodelTests=false` and `-DmodelDocs=false`.
These options default to true and don't limit the generation of the feature options listed above (like `-Dapi`):

```sh
# generate only models (with tests and documentation)
java -Dmodels {opts}

# generate only models (with tests but no documentation)
java -Dmodels -DmodelDocs=false {opts}

# generate only User and Pet models (no tests and no documentation)
java -Dmodels=User,Pet -DmodelTests=false {opts}

# generate only apis (without tests)
java -Dapis -DapiTests=false {opts}

# generate only apis (modelTests option is ignored)
java -Dapis -DmodelTests=false {opts}
```

When using selective generation, _only_ the templates needed for the specific generation will be used.

### Ignore file format

Swagger Codegen supports a `.swagger-codegen-ignore` file, similar to `.gitignore` or `.dockerignore` you're probably already familiar with.

The ignore file allows for better control over overwriting existing files than the `--skip-overwrite` flag. With the ignore file, you can specify individual files or directories can be ignored. This can be useful, for example if you only want a subset of the generated code.

Examples:

```sh
# Swagger Codegen Ignore
# Lines beginning with a # are comments

# This should match build.sh located anywhere.
build.sh

# Matches build.sh in the root
/build.sh

# Exclude all recursively
docs/**

# Explicitly allow files excluded by other rules
!docs/UserApi.md

# Recursively exclude directories named Api
# You can't negate files below this directory.
src/**/Api/

# When this file is nested under /Api (excluded above),
# this rule is ignored because parent directory is excluded by previous rule.
!src/**/PetApiTests.cs

# Exclude a single, nested file explicitly
src/IO.Swagger.Test/Model/AnimalFarmTests.cs
```

The `.swagger-codegen-ignore` file must exist in the root of the output directory.

Upon first code generation, you may also pass the CLI option `--ignore-file-override=/path/to/ignore_file` for greater control over generated outputs. Note that this is a complete override, and will override the `.swagger-codegen-ignore` file in an output directory when regenerating code.

Editor support for `.swagger-codegen-ignore` files is available in IntelliJ via the [.ignore plugin](https://plugins.jetbrains.com/plugin/7495--ignore).

### Customizing the generator

There are different aspects of customizing the code generator (located in this version at [swagger codegen generator repo](https://github.com/swagger-api/swagger-codegen-generators)) beyond just creating or modifying templates.  Each language has a supporting configuration file to handle different type mappings, etc:

```sh
at https://github.com/swagger-api/swagger-codegen-generators/tree/master/src/main/java/io/swagger/codegen/languages/java/

AbstractJavaCodegen.java
AbstractJavaJAXRSServerCodegen.java
...
JavaClientCodegen.java
JavaJAXRSSpecServerCodegen.java
```

Each of these files creates reasonable defaults so you can get running quickly.  But if you want to configure package names, prefixes, model folders, etc. you can use a json config file to pass the values.

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java \
  -c path/to/config.json
```
and `config.json` contains the following as an example:
```json
{
  "apiPackage" : "petstore"
}
```

Supported config options can be different per language. Running `config-help -l {lang}` will show available options.
**These options are applied via configuration file (e.g. config.json) or by passing them with `-D{optionName}={optionValue}`**. (If `-D{optionName}` does not work, please open a [ticket](https://github.com/swagger-api/swagger-codegen/issues/new) and we'll look into it)

```sh
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar config-help -l java
```

Output

```
CONFIG OPTIONS
	modelPackage
	    package for generated models

	apiPackage
	    package for generated api classes
...... (results omitted)
	library
	    library template (sub-template) to use:
	    jersey1 - HTTP client: Jersey client 1.18. JSON processing: Jackson 2.4.2
	    jersey2 - HTTP client: Jersey client 2.6
	    feign - HTTP client: Netflix Feign 8.1.1.  JSON processing: Jackson 2.6.3
	    okhttp-gson (default) - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1
	    retrofit - HTTP client: OkHttp 2.4.0. JSON processing: Gson 2.3.1 (Retrofit 1.9.0)
        retrofit2 - HTTP client: OkHttp 2.5.0. JSON processing: Gson 2.4 (Retrofit 2.0.0-beta2)
        google-api-client - HTTP client: google-api-client 1.23.0. JSON processing: Jackson 2.8.9
```

Your config file for Java can look like

```json
{
  "groupId":"com.my.company",
  "artifactId":"MyClient",
  "artifactVersion":"1.2.0",
  "library":"feign"
}
```

For all the unspecified options default values will be used.

Another way to override default options is to extend the config class for the specific language.
To change, for example, the prefix for the Objective-C generated files, simply subclass the ObjcClientCodegen.java:

```java
package com.mycompany.swagger.codegen;

import io.swagger.codegen.languages.*;

public class MyObjcCodegen extends ObjcClientCodegen {
    static {
        PREFIX = "HELO";
    }
}
```

and specify the `classname` when running the generator:

```
-l com.mycompany.swagger.codegen.MyObjcCodegen
```

Your subclass will now be loaded and overrides the `PREFIX` value in the superclass.

### Bringing your own models

Sometimes you don't want a model generated.  In this case, you can simply specify an import mapping to tell
the codegen what _not_ to create.  When doing this, every location that references a specific model will
refer back to your classes.  Note, this may not apply to all languages...

To specify an import mapping, use the `--import-mappings` argument and specify the model-to-import logic as such:

```
--import-mappings Pet=my.models.MyPet
```

Or for multiple mappings:

```
--import-mappings Pet=my.models.MyPet,Order=my.models.MyOrder
```
or
```
--import-mappings Pet=my.models.MyPet --import-mappings Order=my.models.MyOrder
```


### Validating your OpenAPI Spec

You have options.  The easiest is to use our [online validator](https://github.com/swagger-api/validator-badge) which not only will let you validate your spec, but with the debug flag, you can see what's wrong with your spec.  For example:

http://online.swagger.io/validator/debug?url=http://petstore.swagger.io/v2/swagger.json

### Generating dynamic html api documentation

To do so, just use the `-l dynamic-html` flag when reading a spec file.  This creates HTML documentation that is available as a single-page application with AJAX.  To view the documentation:

```sh
cd samples/dynamic-html/
npm install
node .
```

Which launches a node.js server so the AJAX calls have a place to go.

### To build a server stub

Please refer to https://github.com/swagger-api/swagger-codegen/wiki/Server-stub-generator-HOWTO for more information.

### To build the codegen library

This will create the Swagger Codegen library from source.

```sh
mvn package
```

Note!  The templates are included in the library generated.  If you want to modify the templates, you'll need to either repackage the library OR specify a path to your scripts

## Workflow Integration

### Maven Integration

You can use the [swagger-codegen-maven-plugin](modules/swagger-codegen-maven-plugin/README.md) for integrating with your workflow, and generating any codegen target.

### Gradle Integration

[Gradle Swagger Generator Plugin](https://github.com/int128/gradle-swagger-generator-plugin) is available for generating source code and API document.

## GitHub Integration

To push the auto-generated SDK to GitHub, we provide `git_push.sh` to streamline the process. For example:

 1) Create a new repository in GitHub (Ref: https://help.github.com/articles/creating-a-new-repository/)

 2) Generate the SDK
```sh
 java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
 -i modules/swagger-codegen/src/test/resources/2_0/petstore.json -l perl \
 --git-user-id "swaggerapi" \
 --git-repo-id "petstore-perl" \
 --release-note "Github integration demo" \
 -o /var/tmp/perl/petstore
```
 3) Push the SDK to GitHub
```sh
cd /var/tmp/perl/petstore
/bin/sh ./git_push.sh
```

## Online generators

`swagger-generator` module exposes codegen as a web service, with it's own `swagger-js` based web UI, and available docker image `swaggerapi/swagger-generator-v3`
Such web service is deployed at https://generator3.swagger.io/ui, or it can be easily deployed as docker container.

The OpenAPI specification of generator service APIs are available either via UI exposed by web service (e.g. https://generator3.swagger.io/ui), as exposed YAML (https://generator3.swagger.io/openapi.json)
or in source code repo (https://github.com/swagger-api/swagger-codegen/blob/3.0.0/modules/swagger-generator/src/main/resources/openapi.yaml)

Please note that both V2 (for v2 specs) and V3 generators (for v3 and v2 specs converted during generation) are supported, by providing property `codegenVersion` (e.g `"codegenVersion" : "v3"`)

For example, to generate a java API client, simply send the following HTTP request using curl:
```sh
curl -X POST \
  https://generator3.swagger.io/api/generate \
  -H 'content-type: application/json' \
  -d '{
  "specURL" : "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v3.0/petstore.yaml",
  "lang" : "java",
  "type" : "CLIENT",
  "codegenVersion" : "V3"
}'
```
The response will contain a zipped file containing the generated code.

To customize the SDK, you can specify language specific options  with the following HTTP body:
```json
{
  "specURL" : "https://raw.githubusercontent.com/OAI/OpenAPI-Specification/master/examples/v3.0/petstore.yaml",
  "lang" : "java",
  "options" : {
    "additionalProperties" : {
    	"useRuntimeException": true,
    	"useRxJava" : true
    }
  },
  "type" : "CLIENT",
  "codegenVersion" : "V3"
}
```
in which the `options` `additionalProperties` for a language can be obtained by submitting a `GET` request to `https://generator3.swagger.io/api/options?language={language}&version={codegenVersion}`:

For example, `curl https://generator3.swagger.io/api/options?language=java&version=V3` returns (truncated output):

```json
{
  "sortParamsByRequiredFlag": {
    "opt": "sortParamsByRequiredFlag",
    "description": "Sort method arguments to place required parameters before optional parameters.",
    "type": "boolean",
    "default": "true"
  },
  "ensureUniqueParams": {
    "opt": "ensureUniqueParams",
    "description": "Whether to ensure parameter names are unique in an operation (rename parameters that are not).",
    "type": "boolean",
    "default": "true"
  },
  "allowUnicodeIdentifiers": {
    "opt": "allowUnicodeIdentifiers",
    "description": "boolean, toggles whether unicode identifiers are allowed in names or not, default is false",
    "type": "boolean",
    "default": "false"
  },
  "modelPackage": {
    "opt": "modelPackage",
    "description": "package for generated models",
    "type": "string"
  },
  ...
```

Instead of using `specURL` with an URL to the OpenAPI/Swagger spec, one can include the spec in the JSON payload with `spec`, e.g.
```json
{
  "options": {},
  "spec": {
    "swagger": "2.0",
    "info": {
      "version": "1.0.0",
      "title": "Test API"
    },
    ...
  }
}
```

Guidelines for Contribution
---------------------------

Please refer to this [page.](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md)
Also for `swagger-codegen version 3` templates and classes for code generation are being migrated to [swagger-codegen-generators](https://github.com/swagger-api/swagger-codegen-generators)
repo. In order to know this migration process you can refer this [page.](https://github.com/swagger-api/swagger-codegen/wiki/Swagger-Codegen-migration-(swagger-codegen-generators-repository))

# Security contact

Please disclose any security-related issues or vulnerabilities by emailing [security@swagger.io](mailto:security@swagger.io), instead of using the public issue tracker.

# License information on Generated Code

The Swagger Codegen project is intended as a benefit for users of the Swagger / Open API Specification.  The project itself has the [License](#license) as specified.  In addition, please understand the following points:

* The templates included with this project are subject to the [License](#license).
* Generated code is intentionally _not_ subject to the parent project license

When code is generated from this project, it shall be considered **AS IS** and owned by the user of the software.  There are no warranties--expressed or implied--for generated code.  You can do what you wish with it, and once generated, the code is your responsibility and subject to the licensing terms that you deem appropriate.

# License

```
Copyright 2019 SmartBear Software

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

