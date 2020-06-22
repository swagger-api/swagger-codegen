#!/bin/sh
set -euo pipefail
# GEN_DIR allows to share the entrypoint between Dockerfile and run-in-docker.sh (backward compatible)
GEN_DIR=${GEN_DIR:-/opt/swagger-codegen}
JAVA_OPTS=${JAVA_OPTS:-"-Xmx1024M -Dlogback.configurationFile=conf/logback.xml"}

codegen3="${GEN_DIR}/swagger-codegen-cli.jar"

command=$1
exec java ${JAVA_OPTS} -jar "${codegen3}" "meta" "$@"
