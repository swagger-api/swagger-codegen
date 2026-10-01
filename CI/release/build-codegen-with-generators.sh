#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

generators_version="${1:-}"
[[ -n "${generators_version}" ]] || fail "Usage: build-codegen-with-generators.sh <generators_version>"

# Standardized codegen build command with explicit generators coordinate.
mvn -B -U clean install -Pdocker \
  -Dswagger-codegen-generators-version="${generators_version}" \
  -DJETTY_TEST_HTTP_PORT=8090 \
  -DJETTY_TEST_STOP_PORT=8089
