#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from dataclasses import asdict, dataclass
from pathlib import Path

try:
    import tomllib
except ModuleNotFoundError as exc:  # pragma: no cover
    raise SystemExit(f"error: python tomllib is required: {exc}")


SKIPPED_PATH_PARTS = {"tests", "fixtures", "workspace_fixtures"}


@dataclass
class ManifestInfo:
    path: str
    package_name: str
    version: str


@dataclass
class Inventory:
    repo_root: str
    riot_version: str
    next_patch_version: str
    toolchain_version: str | None
    toolchain_targets: list[str]
    last_semver_tag: str | None
    worktree_dirty: bool
    release_manifests: list[ManifestInfo]
    skipped_manifests: list[ManifestInfo]
    version_mismatches: list[ManifestInfo]


def die(message: str) -> None:
    raise SystemExit(f"error: {message}")


def find_repo_root(start: Path) -> Path:
    for candidate in [start, *start.parents]:
        if (
            (candidate / "packages/riot-cli/riot.toml").is_file()
            and (candidate / "ocaml-toolchain.toml").is_file()
        ):
            return candidate
    die("could not locate riot-new repository root from current directory")


def load_toml(path: Path) -> dict:
    with path.open("rb") as handle:
        data = tomllib.load(handle)
    if not isinstance(data, dict):
        die(f"{path} did not parse to a TOML table")
    return data


def manifest_info(path: Path, repo_root: Path, *, require_package: bool = True) -> ManifestInfo | None:
    data = load_toml(path)
    package = data.get("package")
    if not isinstance(package, dict):
        if require_package:
            die(f"{path} is missing a [package] table")
        return None
    name = package.get("name")
    version = package.get("version")
    if not isinstance(name, str) or not name:
        die(f"{path} is missing package.name")
    if not isinstance(version, str) or not version:
        die(f"{path} is missing package.version")
    return ManifestInfo(
        path=str(path.relative_to(repo_root)),
        package_name=name,
        version=version,
    )


def should_skip_manifest(path: Path) -> bool:
    return any(part in SKIPPED_PATH_PARTS for part in path.parts)


def discover_manifests(repo_root: Path) -> tuple[list[ManifestInfo], list[ManifestInfo]]:
    release_manifests: list[ManifestInfo] = []
    skipped_manifests: list[ManifestInfo] = []

    for top_level in ("packages", "services"):
        root = repo_root / top_level
        if not root.is_dir():
            continue
        for manifest_path in sorted(root.rglob("riot.toml")):
            info = manifest_info(manifest_path, repo_root, require_package=False)
            if info is None:
                continue
            if should_skip_manifest(manifest_path.relative_to(repo_root)):
                skipped_manifests.append(info)
            else:
                release_manifests.append(info)

    return release_manifests, skipped_manifests


def next_patch(version: str) -> str:
    parts = version.split(".")
    if len(parts) != 3 or not all(part.isdigit() for part in parts):
        die(f"expected semver-like Riot version, got {version!r}")
    major, minor, patch = (int(part) for part in parts)
    return f"{major}.{minor}.{patch + 1}"


def git_output(repo_root: Path, *args: str) -> str | None:
    try:
        completed = subprocess.run(
            ["git", "-C", str(repo_root), *args],
            check=True,
            capture_output=True,
            text=True,
        )
    except (FileNotFoundError, subprocess.CalledProcessError):
        return None
    return completed.stdout.strip()


def current_worktree_dirty(repo_root: Path) -> bool:
    output = git_output(repo_root, "status", "--short")
    return bool(output)


def last_semver_tag(repo_root: Path) -> str | None:
    return git_output(repo_root, "describe", "--tags", "--abbrev=0", "--match", "[0-9]*.[0-9]*.[0-9]*")


def toolchain_info(repo_root: Path) -> tuple[str | None, list[str]]:
    data = load_toml(repo_root / "ocaml-toolchain.toml")
    toolchain = data.get("toolchain")
    if not isinstance(toolchain, dict):
        return (None, [])

    version = toolchain.get("version")
    targets = toolchain.get("targets")
    if not isinstance(version, str):
        version = None
    if not isinstance(targets, list):
        return (version, [])

    normalized_targets = [target for target in targets if isinstance(target, str) and target]
    return (version, normalized_targets)


def build_inventory(repo_root: Path) -> Inventory:
    riot_manifest = manifest_info(repo_root / "packages/riot-cli/riot.toml", repo_root)
    if riot_manifest is None:
        die("packages/riot-cli/riot.toml must be a package manifest")
    release_manifests, skipped_manifests = discover_manifests(repo_root)
    toolchain_version, toolchain_targets = toolchain_info(repo_root)
    mismatches = [
        manifest
        for manifest in release_manifests
        if manifest.version != riot_manifest.version
    ]

    return Inventory(
        repo_root=str(repo_root),
        riot_version=riot_manifest.version,
        next_patch_version=next_patch(riot_manifest.version),
        toolchain_version=toolchain_version,
        toolchain_targets=toolchain_targets,
        last_semver_tag=last_semver_tag(repo_root),
        worktree_dirty=current_worktree_dirty(repo_root),
        release_manifests=release_manifests,
        skipped_manifests=skipped_manifests,
        version_mismatches=mismatches,
    )


def print_summary(inventory: Inventory) -> None:
    print(f"Repo root:           {inventory.repo_root}")
    print(f"Riot version:        {inventory.riot_version}")
    print(f"Next patch:          {inventory.next_patch_version}")
    print(f"Toolchain version:   {inventory.toolchain_version or '(missing)'}")
    print(f"Last semver tag:     {inventory.last_semver_tag or '(none)'}")
    print(f"Worktree dirty:      {'yes' if inventory.worktree_dirty else 'no'}")
    print(f"Release manifests:   {len(inventory.release_manifests)}")
    print(f"Skipped manifests:   {len(inventory.skipped_manifests)}")
    print(f"Version mismatches:  {len(inventory.version_mismatches)}")
    print(f"Toolchain targets:   {len(inventory.toolchain_targets)}")


def print_manifest_list(header: str, manifests: list[ManifestInfo]) -> None:
    print(header)
    for manifest in manifests:
        print(f"  - {manifest.path} ({manifest.package_name}@{manifest.version})")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(
        description="Inspect Riot release inputs for this repository."
    )
    parser.add_argument("--json", action="store_true", help="Emit full inventory as JSON.")
    parser.add_argument(
        "--list-manifests",
        action="store_true",
        help="Print the real release manifests that should be version-bumped.",
    )
    parser.add_argument(
        "--list-skipped",
        action="store_true",
        help="Print skipped fixture/test manifests.",
    )
    parser.add_argument(
        "--list-mismatches",
        action="store_true",
        help="Print release manifests whose version differs from packages/riot-cli/riot.toml.",
    )
    args = parser.parse_args(argv)

    repo_root = find_repo_root(Path.cwd())
    inventory = build_inventory(repo_root)

    if args.json:
        print(json.dumps(asdict(inventory), indent=2, sort_keys=True))
        return 0

    print_summary(inventory)

    if args.list_manifests:
        print()
        print_manifest_list("Release manifests:", inventory.release_manifests)

    if args.list_skipped:
        print()
        print_manifest_list("Skipped manifests:", inventory.skipped_manifests)

    if args.list_mismatches:
        print()
        print_manifest_list("Version mismatches:", inventory.version_mismatches)

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
