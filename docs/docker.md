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

See [standalone generator development](https://github.com/swagger-api/swagger-codegen/blob/master/standalone-gen-dev/standalone-generator-development.md)

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

- https://hub.docker.com/r/swaggerapi/swagger-generator/ (official web service)
- https://hub.docker.com/r/swaggerapi/swagger-codegen-cli/ (official CLI)

### Swagger Generator Docker Image

The Swagger Generator image can act as a self-hosted web application and API for generating code. This container can be  incorporated into a CI pipeline, and requires at least two HTTP requests and some docker orchestration to access generated code.

Example usage (note this assumes `jq` is installed for command line processing of JSON):

```sh
# Start container and save the container id
CID=$(docker run -d swaggerapi/swagger-generator)
# allow for startup
sleep 5
# Get the IP of the running container
GEN_IP=$(docker inspect --format '{{.NetworkSettings.IPAddress}}'  $CID)
# Execute an HTTP request and store the download link
RESULT=$(curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' -d '{
  "swaggerUrl": "https://petstore.swagger.io/v2/swagger.json"
}' 'http://localhost:8188/api/gen/clients/javascript' | jq '.link' | tr -d '"')
# Download the generated zip and redirect to a file
curl $RESULT > result.zip
# Shutdown the swagger generator image
docker stop $CID && docker rm $CID
```

In the example above, `result.zip` will contain the generated client.

### Swagger Codegen CLI Docker Image

The Swagger Codegen image acts as a standalone executable. It can be used as an alternative to installing via homebrew, or for developers who are unable to install Java or upgrade the installed version.

To generate code with this image, you'll need to mount a local location as a volume.

Example:

```sh
docker run --rm -v ${PWD}:/local swaggerapi/swagger-codegen-cli generate \
    -i https://petstore.swagger.io/v2/swagger.json \
    -l go \
    -o /local/out/go
```

(On Windows replace `${PWD}` with `%CD%`)

The generated code will be located under `./out/go` in the current directory.
