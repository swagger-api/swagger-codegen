#!/bin/bash
set -exo pipefail

cd "$(dirname ${BASH_SOURCE})"

docker run --rm -it \
        -w /gen \
        -e GEN_DIR=/gen \
        -v "${PWD}:/gen" \
        --entrypoint /gen/docker-stub.sh \
        openjdk:17-jre-alpine "$@"
