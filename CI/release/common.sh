#!/usr/bin/env bash

set -euo pipefail

RELEASED_MAVEN_BASE="${RELEASED_MAVEN_BASE:-https://repo1.maven.org/maven2}"
SNAPSHOT_MAVEN_BASE="${SNAPSHOT_MAVEN_BASE:-https://central.sonatype.com/repository/maven-snapshots}"
CODEGEN_GROUP_PATH="io/swagger/codegen/v3"
GENERATORS_ARTIFACT="swagger-codegen-generators"

fail() {
  echo "::error::$*"
  exit 1
}

require_release_version() {
  local name="$1"
  local version="$2"

  [[ -n "${version}" ]] || fail "${name} is required"
  [[ ! "${version}" =~ SNAPSHOT$ ]] || fail "${name} must be a release version, got ${version}"
  [[ "${version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || fail "${name} must match X.Y.Z, got ${version}"
}

require_release_or_snapshot_version() {
  local name="$1"
  local version="$2"

  [[ -n "${version}" ]] || fail "${name} is required"
  [[ "${version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-SNAPSHOT)?$ ]] || fail "${name} must match X.Y.Z or X.Y.Z-SNAPSHOT, got ${version}"
}

require_snapshot_version() {
  local name="$1"
  local version="$2"

  [[ -n "${version}" ]] || fail "${name} is required"
  [[ "${version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+-SNAPSHOT$ ]] || fail "${name} must match X.Y.Z-SNAPSHOT, got ${version}"
}

release_from_snapshot_version() {
  local version="$1"

  [[ "${version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+-SNAPSHOT$ ]] || fail "Cannot derive release version from non-SNAPSHOT version ${version}"
  printf '%s\n' "${version%-SNAPSHOT}"
}

next_snapshot_from_release_version() {
  local version="$1"
  local major minor patch

  require_release_version "version" "${version}"
  IFS=. read -r major minor patch <<< "${version}"
  printf '%s.%s.%s-SNAPSHOT\n' "${major}" "${minor}" "$((patch + 1))"
}

## Thin Maven helpers for reading project-level values from pom.xml.
maven_project_version() {
  mvn -q -Dexec.executable="echo" -Dexec.args='${project.version}' --non-recursive org.codehaus.mojo:exec-maven-plugin:1.3.1:exec
}

## Shared curl wrapper for metadata fetch with retries and hard timeouts.
curl_metadata() {
  local url="$1"
  curl --fail --silent --show-error --location --retry 5 --retry-delay 2 --connect-timeout 20 --max-time 90 "${url}"
}

## Parse <version> entries from Maven metadata.xml.
versions_from_metadata() {
  awk -F'[<>]' '/<version>/{print $3}'
}

## Resolve the newest version matching a regex pattern from metadata.
latest_matching_version() {
  local metadata_url="$1"
  local pattern="$2"
  local version

  version="$(curl_metadata "${metadata_url}" | versions_from_metadata | grep -E "${pattern}" | sort -V | tail -n 1 || true)"
  [[ -n "${version}" ]] || fail "No version matching '${pattern}' found in ${metadata_url}"
  printf '%s\n' "${version}"
}

latest_released_generators_version() {
  latest_matching_version "${RELEASED_MAVEN_BASE}/${CODEGEN_GROUP_PATH}/${GENERATORS_ARTIFACT}/maven-metadata.xml" '^1\.[0-9]+\.[0-9]+$'
}

latest_snapshot_generators_version() {
  latest_matching_version "${SNAPSHOT_MAVEN_BASE}/${CODEGEN_GROUP_PATH}/${GENERATORS_ARTIFACT}/maven-metadata.xml" '^1\.[0-9]+\.[0-9]+-SNAPSHOT$'
}

## Build canonical artifact URLs and probe existence without downloading payloads.
release_artifact_url() {
  local artifact="$1"
  local version="$2"

  printf '%s/%s/%s/%s/%s-%s.pom\n' "${RELEASED_MAVEN_BASE}" "${CODEGEN_GROUP_PATH}" "${artifact}" "${version}" "${artifact}" "${version}"
}

release_artifact_exists() {
  local artifact="$1"
  local version="$2"
  local url

  url="$(release_artifact_url "${artifact}" "${version}")"
  curl --fail --silent --show-error --head --location --retry 3 --connect-timeout 20 --max-time 60 "${url}" >/dev/null 2>&1
}

## SNAPSHOT coordinates resolve via maven-metadata.xml, not fixed file names.
snapshot_metadata_url() {
  local artifact="$1"
  local version="$2"

  printf '%s/%s/%s/%s/maven-metadata.xml\n' "${SNAPSHOT_MAVEN_BASE}" "${CODEGEN_GROUP_PATH}" "${artifact}" "${version}"
}

assert_snapshot_metadata_exists() {
  local artifact="$1"
  local version="$2"
  local metadata_url

  metadata_url="$(snapshot_metadata_url "${artifact}" "${version}")"
  if ! curl_metadata "${metadata_url}" >/dev/null; then
    fail "Required SNAPSHOT ${CODEGEN_GROUP_PATH}:${artifact}:${version} cannot be resolved from ${metadata_url}. Sonatype snapshots can expire. Recovery: publish that exact snapshot version, then rerun this workflow."
  fi
}
