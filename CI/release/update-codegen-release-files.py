#!/usr/bin/env python3

from pathlib import Path
import re
import sys

COMPATIBILITY_DOCS = ["README.md", "docs/compatibility.md"]
RELEASE_DOCS = ["README.md", "docs/prerequisites.md", "docs/versioning.md"]

# Regex patterns intentionally target release tables/examples used in public docs.
SNAPSHOT_ROW_PATTERN = (
    r"\|?\s*[0-9]+\.[0-9]+\.[0-9]+-SNAPSHOT \(current 3\.0\.0, upcoming minor release\).*?\|\s*Minor release\s*\|?"
)
RELEASE_ROW_PATTERN = (
    r"\| \[[0-9]+\.[0-9]+\.[0-9]+\]\(https://github\.com/swagger-api/swagger-codegen/releases/tag/v[0-9]+\.[0-9]+\.[0-9]+\) "
    r"(?:\(\*\*current stable\*\*\)\s*)?.*?\| \[tag v[0-9]+\.[0-9]+\.[0-9]+\]\(https://github\.com/swagger-api/swagger-codegen/tree/v[0-9]+\.[0-9]+\.[0-9]+\)\s*\|"
)
RELEASE_JAR_PATTERN = (
    r"io/swagger/codegen/v3/swagger-codegen-cli/[0-9]+\.[0-9]+\.[0-9]+/swagger-codegen-cli-[0-9]+\.[0-9]+\.[0-9]+\.jar"
)
SNAPSHOT_JAR_PATTERN = (
    r"io/swagger/codegen/v3/swagger-codegen-cli/[0-9]+\.[0-9]+\.[0-9]+-SNAPSHOT/swagger-codegen-cli-[0-9]+\.[0-9]+\.[0-9]+-SNAPSHOT\.jar"
)
RELEASE_VERSION_PATTERN = r"<version>3\.0\.[0-9]+</version>"
SNAPSHOT_VERSION_PATTERN = r"<version>3\.0\.[0-9]+-SNAPSHOT</version>"


def replace_text(path: str, replacements: list[tuple[str, str]]) -> None:
    # Generic file replacement helper; no-op if target file is absent.
    file_path = Path(path)
    if not file_path.exists():
        return
    text = file_path.read_text()
    original = text
    for pattern, value in replacements:
        text = re.sub(pattern, value, text, flags=re.MULTILINE)
    if text != original:
        file_path.write_text(text)


def replace_text_in_docs(
    docs: list[str], replacements: list[tuple[str, str]], *, count: int = 0, flags: int = re.MULTILINE, require_match: bool = False
) -> None:
    # Apply replacement sets across a known docs list.
    for doc in docs:
        file_path = Path(doc)
        if not file_path.exists():
            continue
        text = file_path.read_text()
        original = text
        matched_any = False
        for pattern, value in replacements:
            text, replaced = re.subn(pattern, value, text, count=count, flags=flags)
            matched_any = matched_any or replaced > 0
        if require_match and not matched_any:
            raise RuntimeError(f"No replacements matched in {doc}")
        if text != original:
            file_path.write_text(text)


def update_generators_poms(generators_version: str) -> None:
    # Keep generators dependency aligned in both root pom variants.
    replacements = [
        (
            r"<swagger-codegen-generators-version>[^<]+</swagger-codegen-generators-version>",
            f"<swagger-codegen-generators-version>{generators_version}</swagger-codegen-generators-version>",
        ),
    ]
    replace_text("pom.xml", replacements)
    replace_text("pom.docker.xml", replacements)


def update_sample_meta_codegen_pom(codegen_version: str) -> None:
    # Keep sample project pinned to the codegen version being prepared.
    replace_text(
        "samples/meta-codegen/pom.xml",
        [(r"<swagger-codegen-version>[^<]+</swagger-codegen-version>", f"<swagger-codegen-version>{codegen_version}</swagger-codegen-version>")],
    )


def update_docker_pom_version(codegen_version: str) -> None:
    # Keep root project version in pom.docker.xml aligned with released codegen version.
    replace_text(
        "pom.docker.xml",
        [(r"<version>3\.0\.[0-9]+(?:-SNAPSHOT)?</version>", f"<version>{codegen_version}</version>")],
    )


def update_openapi_version(version: str) -> None:
    # Reflect current codegen version in online generator OpenAPI metadata.
    replace_text(
        "modules/swagger-generator/src/main/resources/openapi.yaml",
        [(r"^  version: .*$", f"  version: {version}")],
    )


def update_snapshot_rows(next_snapshot: str) -> None:
    # Update "current upcoming snapshot" row in compatibility docs.
    snapshot_row = (
        f"{next_snapshot} (current 3.0.0, upcoming minor release) "
        "[SNAPSHOT](https://central.sonatype.com/service/rest/repository/browse/maven-snapshots/"
        f"io/swagger/codegen/v3/swagger-codegen-cli/{next_snapshot}/)| TBD          | 1.0, 1.1, 1.2, 2.0, 3.0 | Minor release"
    )
    replace_text_in_docs(COMPATIBILITY_DOCS, [(SNAPSHOT_ROW_PATTERN, snapshot_row)], require_match=True)


def update_release_rows(codegen_version: str) -> None:
    # Mark latest stable release row at the top of compatibility tables.
    release_row = (
        f"| [{codegen_version}](https://github.com/swagger-api/swagger-codegen/releases/tag/v{codegen_version}) "
        "(**current stable**) | TBD          | 1.0, 1.1, 1.2, 2.0, 3.0              | "
        f"[tag v{codegen_version}](https://github.com/swagger-api/swagger-codegen/tree/v{codegen_version}) |"
    )
    replace_text_in_docs(COMPATIBILITY_DOCS, [(RELEASE_ROW_PATTERN, release_row)], count=1)


def update_release_docs(codegen_version: str) -> None:
    # Point release examples to concrete released CLI coordinates.
    replace_text_in_docs(
        RELEASE_DOCS,
        [
            (
                RELEASE_JAR_PATTERN,
                f"io/swagger/codegen/v3/swagger-codegen-cli/{codegen_version}/swagger-codegen-cli-{codegen_version}.jar",
            ),
            (RELEASE_VERSION_PATTERN, f"<version>{codegen_version}</version>"),
        ],
    )


def update_snapshot_docs(next_snapshot: str) -> None:
    # Point snapshot examples to next development CLI coordinates.
    replace_text_in_docs(
        RELEASE_DOCS,
        [
            (
                SNAPSHOT_JAR_PATTERN,
                f"io/swagger/codegen/v3/swagger-codegen-cli/{next_snapshot}/swagger-codegen-cli-{next_snapshot}.jar",
            ),
            (SNAPSHOT_VERSION_PATTERN, f"<version>{next_snapshot}</version>"),
        ],
    )


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: update-codegen-release-files.py <prepare|post> [...]", file=sys.stderr)
        return 2

    mode = sys.argv[1]
    if mode == "prepare":
        # Prepare mode: move docs/content to released codegen version state.
        if len(sys.argv) != 5:
            print("usage: ... prepare <codegen_version> <next_snapshot> <generators_version>", file=sys.stderr)
            return 2
        codegen_version, next_snapshot, generators_version = sys.argv[2:5]
        update_generators_poms(generators_version)
        update_sample_meta_codegen_pom(codegen_version)
        update_docker_pom_version(codegen_version)
        update_openapi_version(codegen_version)
        update_snapshot_rows(next_snapshot)
        update_release_rows(codegen_version)
        update_release_docs(codegen_version)
        return 0

    if mode == "post":
        # Post mode: move docs/content back to next snapshot development state.
        if len(sys.argv) != 4:
            print("usage: ... post <next_snapshot> <generators_version>", file=sys.stderr)
            return 2
        next_snapshot, generators_version = sys.argv[2:4]
        update_generators_poms(generators_version)
        update_openapi_version(next_snapshot)
        update_snapshot_rows(next_snapshot)
        update_snapshot_docs(next_snapshot)
        return 0

    print(f"unknown mode: {mode}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
