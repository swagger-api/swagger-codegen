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

current_version="$(maven_project_version)"
[[ "${current_version}" =~ SNAPSHOT$ ]] || fail "Prepare release must start from a SNAPSHOT codegen version, got ${current_version}"

if [[ -z "${codegen_version}" ]]; then
  codegen_version="$(release_from_snapshot_version "${current_version}")"
fi
require_release_version "CODEGEN_VERSION" "${codegen_version}"

if [[ -z "${next_codegen_snapshot_version}" ]]; then
  next_codegen_snapshot_version="$(next_snapshot_from_release_version "${codegen_version}")"
fi
require_snapshot_version "NEXT_CODEGEN_SNAPSHOT_VERSION" "${next_codegen_snapshot_version}"

if [[ "${release_generators}" == "true" ]]; then
  require_release_version "GENERATORS_VERSION" "${generators_version}"
  if [[ -n "${previous_generators_version}" ]]; then
    require_release_or_snapshot_version "PREVIOUS_GENERATORS_VERSION" "${previous_generators_version}"
    build_generators_version="${previous_generators_version}"
  else
    build_generators_version="$(latest_snapshot_generators_version)"
  fi
else
  if [[ -z "${generators_version}" ]]; then
    generators_version="$(latest_released_generators_version)"
  fi
  require_release_version "resolved generators version" "${generators_version}"
  build_generators_version="${generators_version}"
fi

if [[ "${build_generators_version}" =~ SNAPSHOT$ ]]; then
  assert_snapshot_metadata_exists "${GENERATORS_ARTIFACT}" "${build_generators_version}"
else
  release_artifact_exists "${GENERATORS_ARTIFACT}" "${build_generators_version}" || fail "Generator release ${build_generators_version} does not exist in Maven Central"
fi

echo "Preparing codegen ${codegen_version} from ${current_version}"
echo "Using swagger-codegen-generators ${generators_version}"
echo "Building release candidate with swagger-codegen-generators ${build_generators_version}"

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

mvn -B versions:set -DnewVersion="${codegen_version}"
mvn -B versions:commit

mkdir -p docs/release-notes
previous_tag="$(git tag --merged HEAD --list 'v3.*' | sort -V | tail -n 1 || true)"
release_notes_file="docs/release-notes/v${codegen_version}.md"
{
  echo "# Swagger Codegen ${codegen_version}"
  echo
  echo "Tag: v${codegen_version}"
  if [[ -n "${previous_tag}" ]]; then
    echo "Compared from: ${previous_tag}"
    echo
    echo "## Commits"
    git log --first-parent --pretty=format:'- %h %s' "${previous_tag}..HEAD"
    echo
  else
    echo "Compared from: initial 3.x history"
    echo
    echo "## Commits"
    git log --first-parent --pretty=format:'- %h %s' HEAD
    echo
  fi
} > "${release_notes_file}"

python3 - "$codegen_version" "$next_codegen_snapshot_version" "$generators_version" <<'PY'
from pathlib import Path
import re
import sys

codegen_version, next_snapshot, generators_version = sys.argv[1:4]

def replace(path, replacements):
    file_path = Path(path)
    if not file_path.exists():
        return
    text = file_path.read_text()
    original = text
    for pattern, value in replacements:
        text = re.sub(pattern, value, text, flags=re.MULTILINE)
    if text != original:
        file_path.write_text(text)

replace("pom.xml", [
    (r"<swagger-codegen-generators-version>[^<]+</swagger-codegen-generators-version>", f"<swagger-codegen-generators-version>{generators_version}</swagger-codegen-generators-version>"),
])

replace("pom.docker.xml", [
    (r"<swagger-codegen-generators-version>[^<]+</swagger-codegen-generators-version>", f"<swagger-codegen-generators-version>{generators_version}</swagger-codegen-generators-version>"),
])

replace("modules/swagger-generator/src/main/resources/openapi.yaml", [
    (r"^  version: .*$", f"  version: {codegen_version}"),
])

release_row = f"| [{codegen_version}](https://github.com/swagger-api/swagger-codegen/releases/tag/v{codegen_version}) (**current stable**) | TBD          | 1.0, 1.1, 1.2, 2.0, 3.0              | [tag v{codegen_version}](https://github.com/swagger-api/swagger-codegen/tree/v{codegen_version}) |"
snapshot_row = f"| {next_snapshot} (current 3.0.0, upcoming minor release) [SNAPSHOT](https://central.sonatype.com/service/rest/repository/browse/maven-snapshots/io/swagger/codegen/v3/swagger-codegen-cli/{next_snapshot}/) | TBD          | 1.0, 1.1, 1.2, 2.0, 3.0              | Minor release |"

for doc in ["README.md", "docs/compatibility.md"]:
    path = Path(doc)
    if not path.exists():
        continue
    text = path.read_text()
    text = re.sub(r"\| [0-9]+\.[0-9]+\.[0-9]+-SNAPSHOT \(current 3\.0\.0, upcoming minor release\).*?\| Minor release \|", snapshot_row, text)
    text = re.sub(r"\| \[[0-9]+\.[0-9]+\.[0-9]+\]\(https://github\.com/swagger-api/swagger-codegen/releases/tag/v[0-9]+\.[0-9]+\.[0-9]+\) \(\*\*current stable\*\*\).*?\| \[tag v[0-9]+\.[0-9]+\.[0-9]+\]\(https://github\.com/swagger-api/swagger-codegen/tree/v[0-9]+\.[0-9]+\.[0-9]+\) \|", release_row, text, count=1)
    path.write_text(text)

for doc in ["README.md", "docs/prerequisites.md", "docs/versioning.md"]:
    path = Path(doc)
    if not path.exists():
        continue
    text = path.read_text()
    text = re.sub(r"io/swagger/codegen/v3/swagger-codegen-cli/[0-9]+\.[0-9]+\.[0-9]+/swagger-codegen-cli-[0-9]+\.[0-9]+\.[0-9]+\.jar", f"io/swagger/codegen/v3/swagger-codegen-cli/{codegen_version}/swagger-codegen-cli-{codegen_version}.jar", text)
    text = re.sub(r"<version>3\.0\.[0-9]+</version>", f"<version>{codegen_version}</version>", text)
    path.write_text(text)
PY

echo "Prepared release file updates for ${codegen_version}"
