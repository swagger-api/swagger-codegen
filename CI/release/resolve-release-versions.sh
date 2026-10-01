#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

release_generators="${RELEASE_GENERATORS:-false}"
generators_version_override="${GENERATORS_VERSION_OVERRIDE:-}"
previous_generators_version="${PREVIOUS_GENERATORS_VERSION:-}"
bootstrap_codegen_version="${CODEGEN_GENERATORS_BOOTSTRAP_VERSION:-}"
generators_existing_behavior="${GENERATORS_EXISTING_BEHAVIOR:-skip}"
skip_maven_deploy="${SKIP_MAVEN_DEPLOY:-false}"
dry_run="${DRY_RUN:-false}"
next_codegen_snapshot_version="${NEXT_CODEGEN_SNAPSHOT_VERSION:-}"

# Guard critical feature flags early to fail fast on bad inputs.
[[ "${release_generators}" == "true" || "${release_generators}" == "false" ]] || fail "RELEASE_GENERATORS must be true or false"
[[ "${generators_existing_behavior}" == "skip" || "${generators_existing_behavior}" == "fail" ]] || fail "GENERATORS_EXISTING_BEHAVIOR must be skip or fail"

# Release workflow expects prepare PR already merged (non-SNAPSHOT project version).
codegen_version="$(maven_project_version)"
require_release_version "codegen POM version" "${codegen_version}"

if [[ -z "${next_codegen_snapshot_version}" ]]; then
  next_codegen_snapshot_version="$(next_snapshot_from_release_version "${codegen_version}")"
fi
require_snapshot_version "NEXT_CODEGEN_SNAPSHOT_VERSION" "${next_codegen_snapshot_version}"

codegen_exists="false"
# Protect against accidental redeploy of existing releases.
if release_artifact_exists "${CODEGEN_ARTIFACT}" "${codegen_version}"; then
  codegen_exists="true"
  if [[ "${dry_run}" == "true" ]]; then
    notice "Codegen artifact ${CODEGEN_ARTIFACT}:${codegen_version} already exists; continuing because DRY_RUN=true."
  elif [[ "${skip_maven_deploy}" != "true" ]]; then
    fail "Codegen artifact ${CODEGEN_ARTIFACT}:${codegen_version} already exists in Maven Central. Rerun with skip_maven_deploy=true only for recovery."
  fi
fi

generators_version=""
generators_deploy_needed="false"
codegen_build_generators_version=""
bootstrap_generators_version=""

if [[ "${release_generators}" == "true" ]]; then
  # Generators release path: determine if generators must be published this run.
  [[ -d generators-repo ]] || fail "generators-repo checkout is required when release_generators=true"
  if [[ -n "${bootstrap_codegen_version}" ]]; then
    require_release_or_snapshot_version "codegen_generators_bootstrap_version" "${bootstrap_codegen_version}"
  else
    bootstrap_codegen_version="$(latest_snapshot_codegen_version)"
  fi
  generators_version="$(
    cd generators-repo && \
    mvn -q -Dswagger-codegen-version="${bootstrap_codegen_version}" \
      -Dexec.executable="echo" -Dexec.args='${project.version}' --non-recursive \
      org.codehaus.mojo:exec-maven-plugin:1.3.1:exec
  )"
  require_release_version "swagger-codegen-generators POM version" "${generators_version}"

  if release_artifact_exists "${GENERATORS_ARTIFACT}" "${generators_version}"; then
    if [[ "${generators_existing_behavior}" == "fail" && "${skip_maven_deploy}" != "true" ]]; then
      fail "Generator artifact ${GENERATORS_ARTIFACT}:${generators_version} already exists in Maven Central"
    fi
    generators_deploy_needed="false"
    notice "Generator artifact ${generators_version} already exists; generator deploy will be skipped."
  else
    generators_deploy_needed="true"
    notice "Generator artifact ${generators_version} does not exist; generator deploy is required."
  fi

  if [[ "${generators_deploy_needed}" == "true" && "${skip_maven_deploy}" == "true" && "${dry_run}" != "true" ]]; then
    fail "Generator artifact ${generators_version} does not exist, but skip_maven_deploy=true would skip publishing it. Publish generators first or rerun without skip_maven_deploy."
  fi

  if [[ "${generators_deploy_needed}" == "true" ]]; then
    # Circular dependency bootstrap checks: codegen <-> generators.
    require_release_or_snapshot_version "codegen_generators_bootstrap_version" "${bootstrap_codegen_version}"

    if [[ "${bootstrap_codegen_version}" =~ SNAPSHOT$ ]]; then
      assert_snapshot_metadata_exists "${CODEGEN_ARTIFACT}" "${bootstrap_codegen_version}"
    elif ! release_artifact_exists "${CODEGEN_ARTIFACT}" "${bootstrap_codegen_version}"; then
      fail "Bootstrap codegen release ${bootstrap_codegen_version} does not exist in Maven Central"
    fi

    if [[ -n "${previous_generators_version}" ]]; then
      bootstrap_generators_version="${previous_generators_version}"
    else
      bootstrap_generators_version="$(latest_snapshot_generators_version)"
    fi
    require_release_or_snapshot_version "previous_generators_version" "${bootstrap_generators_version}"

    if [[ "${bootstrap_generators_version}" =~ SNAPSHOT$ ]]; then
      assert_snapshot_metadata_exists "${GENERATORS_ARTIFACT}" "${bootstrap_generators_version}"
    elif ! release_artifact_exists "${GENERATORS_ARTIFACT}" "${bootstrap_generators_version}"; then
      fail "Bootstrap generators release ${bootstrap_generators_version} does not exist in Maven Central"
    fi
    assert_maven_resolves "io.swagger.codegen.v3:${GENERATORS_ARTIFACT}:${bootstrap_generators_version}"
  else
    bootstrap_codegen_version=""
    bootstrap_generators_version="${generators_version}"
  fi

  if [[ "${generators_deploy_needed}" == "true" && "${dry_run}" == "true" ]]; then
    # Dry run cannot use not-yet-published generators release.
    codegen_build_generators_version="${bootstrap_generators_version}"
    notice "Dry run will build codegen with bootstrap generators ${codegen_build_generators_version}; release deploy will use ${generators_version} after generators are published."
  else
    codegen_build_generators_version="${generators_version}"
  fi
else
  # Codegen-only path: generators must already be a resolvable release.
  if [[ -n "${generators_version_override}" ]]; then
    generators_version="${generators_version_override}"
    require_release_version "generators_version_override" "${generators_version}"
  else
    generators_version="$(maven_property_value "swagger-codegen-generators-version")"
    require_release_version "swagger-codegen-generators-version from pom.xml" "${generators_version}"
  fi
  bootstrap_generators_version="${generators_version}"

  if ! release_artifact_exists "${GENERATORS_ARTIFACT}" "${generators_version}"; then
    fail "Resolved generator release ${generators_version} does not exist in Maven Central"
  fi
fi

if [[ "${generators_deploy_needed}" == "true" ]]; then
  notice "Skipping Maven resolve check for ${GENERATORS_ARTIFACT}:${generators_version} because this workflow will publish it before codegen deploy."
else
  assert_maven_resolves "io.swagger.codegen.v3:${GENERATORS_ARTIFACT}:${generators_version}"
fi

# Normalize fallback to avoid empty output in edge branches.
if [[ -z "${codegen_build_generators_version}" ]]; then
  codegen_build_generators_version="${generators_version}"
fi

notice "Resolved codegen_version=${codegen_version}"
notice "Resolved generators_version=${generators_version}"
notice "Resolved codegen_build_generators_version=${codegen_build_generators_version}"
notice "Resolved bootstrap_generators_version=${bootstrap_generators_version}"
notice "Resolved bootstrap_codegen_version=${bootstrap_codegen_version:-none}"
notice "Resolved next_codegen_snapshot_version=${next_codegen_snapshot_version}"
notice "Resolved codegen_exists=${codegen_exists}"
notice "Resolved generators_deploy_needed=${generators_deploy_needed}"

## Export one canonical set of resolved versions to all downstream jobs.
set_output codegen_version "${codegen_version}"
set_output generators_version "${generators_version}"
set_output codegen_build_generators_version "${codegen_build_generators_version}"
set_output bootstrap_generators_version "${bootstrap_generators_version}"
set_output bootstrap_codegen_version "${bootstrap_codegen_version}"
set_output next_codegen_snapshot_version "${next_codegen_snapshot_version}"
set_output codegen_exists "${codegen_exists}"
set_output generators_deploy_needed "${generators_deploy_needed}"
