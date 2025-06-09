# Docker

## Development in docker

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

## Standalone generator Development in docker

See [standalone generator development](https://github.com/swagger-api/swagger-codegen/blob/3.0.0/standalone-gen-dev/standalone-generator-development.md)

## Run Docker in Vagrant

Prerequisite: install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads).

 ```sh
git clone http://github.com/swagger-api/swagger-codegen.git
cd swagger-codegen
vagrant up
vagrant ssh
cd /vagrant
./run-in-docker.sh mvn package
 ```

## Public Pre-built Docker images

- [Official web service](https://hub.docker.com/r/swaggerapi/swagger-generator-v3/)
- [Official web service with "standard" user](https://hub.docker.com/r/swaggerapi/swagger-generator-v3-root/)
- [Official minimal web service](https://hub.docker.com/r/swaggerapi/swagger-generator-v3-minimal/)
- [Official CLI](https://hub.docker.com/r/swaggerapi/swagger-codegen-cli-v3/)

### Swagger Generator Docker Image

The Swagger Generator image provides a ready to use web application (swagger-generator) providing code generation services.

Image accepts the following env variables:

- `JAVA_MEM` e.g. `1024m`
- `HTTP_PORT` e.g. `8080`
- `HIDDEN_OPTIONS_PATH` (alternative to `HIDDEN_OPTIONS`): useful if attaching a volume containing a `hiddenOptions.yaml` file definining which languages to hide. e.g. `/data/hiddenOptions.yaml`
- `HIDDEN_OPTIONS` (alternative to `HIDDEN_OPTIONS_PATH`): allows to pass hidden options as an env variable, in the format `{category}:{language},{language},{language}|{category}:{language},{language},{language}`
e.g. `servers:foo,bar|clientsV3:wtf,isthis` where category can be `clients`, `servers`, `clientsV3`, `serversV3`
- `INFLECTOR_CONFIG_PATH`: the path to a custom [Swagger Inflector configuration](https://github.com/swagger-api/swagger-inflector#configuration), e.g. useful for overriding `rootPath` when exposing Swagger Codegen behind a reverse proxy.

An example of running the container:

`docker pull swaggerapi/swagger-generator-v3`

`docker run -e "HIDDEN_OPTIONS=servers:foo,bar|clientsV3:fgf,sdsd" -e "JAVA_MEM=1024m" -e "HTTP_PORT=80" -p 80:80 --name swagger-generator-v3 -v /tmp:/jetty_home/lib/shared swaggerapi/swagger-generator-v3`

or

`docker run -e "HIDDEN_OPTIONS_PATH=/hiddenOptions.yaml" -e "JAVA_MEM=1024m" -e "HTTP_PORT=80" -p 80:80 --name swagger-generator-v3  -v /tmp:/jetty_home/lib/shared swaggerapi/swagger-generator-v3`

This docker image supports custom generators by dropping the generator jar into `/jetty_home/lib/shared` directory (typically via a docker volume); e.g having on host `/my/custom/coolgenerator.jar` and `/my/custom/weirdgenerator.jar`  the following would have them added to generator service generators:

`docker run -e "HIDDEN_OPTIONS_PATH=/hiddenOptions.yaml" -e "JAVA_MEM=1024m" -e "HTTP_PORT=80" -p 80:80 --name swagger-generator-v3 -v /my/custom:/jetty_home/lib/shared swaggerapi/swagger-generator-v3`

Please note that up to version 3.0.20 you need to provide`-v /{WHATEVER_DIR}:/jetty_home/lib/shared` even if not using custom generators.

See also [online generators](./online-generators.md).

### Swagger Generator "Minimal" Docker Image

The Swagger Generator "Minimal" image can act as a self-hosted web application and API for generating code.

This container can be incorporated into a CI pipeline, and requires some docker orchestration to access generated code.

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

See also [online generators](../README.md#online-generators).

### Swagger Codegen CLI Docker Image

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
