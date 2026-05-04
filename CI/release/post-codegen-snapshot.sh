#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

next_codegen_snapshot_version="${NEXT_CODEGEN_SNAPSHOT_VERSION:-}"
generators_version="${GENERATORS_VERSION:-}"

if [[ -z "${next_codegen_snapshot_version}" ]]; then
  current_version="$(maven_project_version)"
  require_release_version "current codegen POM version" "${current_version}"
  next_codegen_snapshot_version="$(next_snapshot_from_release_version "${current_version}")"
fi
require_snapshot_version "NEXT_CODEGEN_SNAPSHOT_VERSION" "${next_codegen_snapshot_version}"
[[ -n "${generators_version}" ]] || fail "GENERATORS_VERSION is required"

if [[ -n "${GITHUB_ENV:-}" ]]; then
  echo "NEXT_CODEGEN_SNAPSHOT_VERSION=${next_codegen_snapshot_version}" >> "${GITHUB_ENV}"
fi

if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  echo "next_codegen_snapshot_version=${next_codegen_snapshot_version}" >> "${GITHUB_OUTPUT}"
fi

mvn -B versions:set -DnewVersion="${next_codegen_snapshot_version}"
mvn -B versions:commit

python3 - "$next_codegen_snapshot_version" "$generators_version" <<'PY'
from pathlib import Path
import re
import sys

next_snapshot, generators_version = sys.argv[1:3]

for pom in ["pom.xml", "pom.docker.xml"]:
    path = Path(pom)
    if not path.exists():
        continue
    text = path.read_text()
    text = re.sub(r"<swagger-codegen-generators-version>[^<]+</swagger-codegen-generators-version>", f"<swagger-codegen-generators-version>{generators_version}</swagger-codegen-generators-version>", text)
    path.write_text(text)

path = Path("modules/swagger-generator/src/main/resources/openapi.yaml")
if path.exists():
    text = path.read_text()
    text = re.sub(r"^  version: .*$", f"  version: {next_snapshot}", text, flags=re.MULTILINE)
    path.write_text(text)

snapshot_row = f"| {next_snapshot} (current 3.0.0, upcoming minor release) [SNAPSHOT](https://central.sonatype.com/service/rest/repository/browse/maven-snapshots/io/swagger/codegen/v3/swagger-codegen-cli/{next_snapshot}/) | TBD          | 1.0, 1.1, 1.2, 2.0, 3.0              | Minor release |"
for doc in ["README.md", "docs/compatibility.md"]:
    path = Path(doc)
    if not path.exists():
        continue
    text = path.read_text()
    text = re.sub(r"\| [0-9]+\.[0-9]+\.[0-9]+-SNAPSHOT \(current 3\.0\.0, upcoming minor release\).*?\| Minor release \|", snapshot_row, text)
    path.write_text(text)
PY

echo "Prepared next snapshot file updates for ${next_codegen_snapshot_version}"
