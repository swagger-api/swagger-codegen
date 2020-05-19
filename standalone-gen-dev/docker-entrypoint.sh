#!/usr/bin/env bash

set -euo pipefail

# GEN_DIR allows to share the entrypoint between Dockerfile and run-in-docker.sh (backward compatible)
GEN_DIR=${GEN_DIR:-/opt/swagger-codegen}
JAVA_OPTS=${JAVA_OPTS:-"-Xmx1024M -Dlogback.configurationFile=conf/logback.xml"}

cli="${GEN_DIR}"
codegen2="${cli}/swagger-codegen-cli.jar"

(cd "${GEN_DIR}" && exec mvn -Duser.home=$(dirname MAVEN_CONFIG) package)
command=$1
shift
exec java ${JAVA_OPTS} -cp "${codegen2}:${GEN_DIR}/target/*" "io.swagger.codegen.SwaggerCodegen" "${command}" "$@"

