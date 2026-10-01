#!/bin/bash

CUR=$(pwd)

SC_RELEASE_TAG="v$SC_VERSION"

echo "docker tag:"
echo "$SC_RELEASE_TAG"

export DOCKER_VALIDATOR_IMAGE_NAME=swaggerapi/swagger-validator-v2
docker build --rm=false -t $DOCKER_VALIDATOR_IMAGE_NAME:$SC_RELEASE_TAG .
docker tag $DOCKER_VALIDATOR_IMAGE_NAME:$SC_RELEASE_TAG $DOCKER_VALIDATOR_IMAGE_NAME:latest
docker push $DOCKER_VALIDATOR_IMAGE_NAME:$SC_RELEASE_TAG
docker push $DOCKER_VALIDATOR_IMAGE_NAME:latest
echo "docker images:"
docker images | grep -i validator
