#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

next_codegen_snapshot_version="${NEXT_CODEGEN_SNAPSHOT_VERSION:-}"
generators_version="${GENERATORS_VERSION:-}"

# If not provided by workflow input, derive next snapshot from current release.
if [[ -z "${next_codegen_snapshot_version}" ]]; then
  current_version="$(maven_project_version)"
  require_release_version "current codegen POM version" "${current_version}"
  next_codegen_snapshot_version="$(next_snapshot_from_release_version "${current_version}")"
fi
require_snapshot_version "NEXT_CODEGEN_SNAPSHOT_VERSION" "${next_codegen_snapshot_version}"
[[ -n "${generators_version}" ]] || fail "GENERATORS_VERSION is required"

# Expose resolved snapshot version to subsequent workflow steps.
if [[ -n "${GITHUB_ENV:-}" ]]; then
  echo "NEXT_CODEGEN_SNAPSHOT_VERSION=${next_codegen_snapshot_version}" >> "${GITHUB_ENV}"
fi

if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  echo "next_codegen_snapshot_version=${next_codegen_snapshot_version}" >> "${GITHUB_OUTPUT}"
fi

## Return project to snapshot line after release completion.
mvn -B versions:set -DnewVersion="${next_codegen_snapshot_version}"
mvn -B versions:commit

update_codegen_release_files_script="CI/release/update-codegen-release-files.py"
[[ -f "${update_codegen_release_files_script}" ]] || fail "Missing ${update_codegen_release_files_script}"

## Synchronize docs/poms/openapi snapshot references.
python3 "${update_codegen_release_files_script}" post \
  "${next_codegen_snapshot_version}" \
  "${generators_version}"

echo "Prepared next snapshot file updates for ${next_codegen_snapshot_version}"
