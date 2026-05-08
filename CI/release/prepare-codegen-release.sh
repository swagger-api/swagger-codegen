#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

codegen_version="${CODEGEN_VERSION:-}"
next_codegen_snapshot_version="${NEXT_CODEGEN_SNAPSHOT_VERSION:-}"
release_generators="${RELEASE_GENERATORS:-false}"
generators_version="${GENERATORS_VERSION:-}"
previous_generators_version="${PREVIOUS_GENERATORS_VERSION:-}"
build_generators_version=""

# Prepare flow must start from a SNAPSHOT on branch 3.0.0.
current_version="$(maven_project_version)"
[[ "${current_version}" =~ SNAPSHOT$ ]] || fail "Prepare release must start from a SNAPSHOT codegen version, got ${current_version}"

## Resolve target release + next snapshot versions when not provided explicitly.
if [[ -z "${codegen_version}" ]]; then
  codegen_version="$(release_from_snapshot_version "${current_version}")"
fi
require_release_version "CODEGEN_VERSION" "${codegen_version}"

if [[ -z "${next_codegen_snapshot_version}" ]]; then
  next_codegen_snapshot_version="$(next_snapshot_from_release_version "${codegen_version}")"
fi
require_snapshot_version "NEXT_CODEGEN_SNAPSHOT_VERSION" "${next_codegen_snapshot_version}"

if [[ "${release_generators}" == "true" ]]; then
  # Releasing generators: release version is explicit, build can bootstrap from previous snapshot/release.
  require_release_version "GENERATORS_VERSION" "${generators_version}"
  if [[ -n "${previous_generators_version}" ]]; then
    require_release_or_snapshot_version "PREVIOUS_GENERATORS_VERSION" "${previous_generators_version}"
    build_generators_version="${previous_generators_version}"
  else
    build_generators_version="$(latest_snapshot_generators_version)"
  fi
else
  # Not releasing generators: pin codegen to an already released generators artifact.
  if [[ -z "${generators_version}" ]]; then
    generators_version="$(latest_released_generators_version)"
  fi
  require_release_version "resolved generators version" "${generators_version}"
  build_generators_version="${generators_version}"
fi

## Validate the exact generators coordinate used for the candidate build.
if [[ "${build_generators_version}" =~ SNAPSHOT$ ]]; then
  assert_snapshot_metadata_exists "${GENERATORS_ARTIFACT}" "${build_generators_version}"
else
  release_artifact_exists "${GENERATORS_ARTIFACT}" "${build_generators_version}" || fail "Generator release ${build_generators_version} does not exist in Maven Central"
fi

echo "Preparing codegen ${codegen_version} from ${current_version}"
echo "Using swagger-codegen-generators ${generators_version}"
echo "Building release candidate with swagger-codegen-generators ${build_generators_version}"

## Expose resolved values to later workflow steps and PR metadata.
if [[ -n "${GITHUB_ENV:-}" ]]; then
  echo "GENERATORS_VERSION=${generators_version}" >> "${GITHUB_ENV}"
  echo "BUILD_GENERATORS_VERSION=${build_generators_version}" >> "${GITHUB_ENV}"
  echo "CODEGEN_VERSION=${codegen_version}" >> "${GITHUB_ENV}"
  echo "NEXT_CODEGEN_SNAPSHOT_VERSION=${next_codegen_snapshot_version}" >> "${GITHUB_ENV}"
fi

if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  echo "generators_version=${generators_version}" >> "${GITHUB_OUTPUT}"
  echo "build_generators_version=${build_generators_version}" >> "${GITHUB_OUTPUT}"
  echo "codegen_version=${codegen_version}" >> "${GITHUB_OUTPUT}"
  echo "next_codegen_snapshot_version=${next_codegen_snapshot_version}" >> "${GITHUB_OUTPUT}"
fi

## Move project from snapshot to release version before file-level content updates.
mvn -B versions:set -DnewVersion="${codegen_version}"
mvn -B versions:commit

# Generate a minimal release-notes draft aligned with current GitHub release style.
mkdir -p docs/release-notes
previous_tag="$(git tag --merged HEAD --list 'v3.*' | sort -V | tail -n 1 || true)"
release_notes_file="docs/release-notes/v${codegen_version}.md"
{
  echo "# Swagger Codegen v${codegen_version}"
  echo
  echo "## What's Changed"
  echo
  if [[ -n "${previous_tag}" ]]; then
    git log --first-parent --pretty=format:'* %s' "${previous_tag}..HEAD"
    echo
    echo
    echo "Full Changelog: ${previous_tag}...v${codegen_version}"
  else
    git log --first-parent --pretty=format:'* %s' HEAD
    echo
    echo
    echo "Full Changelog: initial...v${codegen_version}"
    echo
  fi
} > "${release_notes_file}"

update_codegen_release_files_script="CI/release/update-codegen-release-files.py"
[[ -f "${update_codegen_release_files_script}" ]] || fail "Missing ${update_codegen_release_files_script}"

## Keep docs/poms/openapi in sync with the release state.
python3 "${update_codegen_release_files_script}" prepare \
  "${codegen_version}" \
  "${next_codegen_snapshot_version}" \
  "${generators_version}"

echo "Prepared release file updates for ${codegen_version}"
