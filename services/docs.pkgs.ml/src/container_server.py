#!/usr/bin/env python3

import base64
import json
import mimetypes
import os
import shutil
import subprocess
import tarfile
import tempfile
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

HOST = "0.0.0.0"
PORT = int(os.environ.get("PORT", "8080"))
DEFAULT_TOOLCHAIN_VERSION = "5.5.0-riot.2"
DEFAULT_TARGET = "x86_64-unknown-linux-gnu"
RIOT_BIN = Path.home() / ".riot" / "bin" / "riot"
DEFAULT_RIOT_INSTALL_URL = "https://get.riot.ml"


def json_bytes(payload):
    return json.dumps(payload).encode("utf-8")


def ensure_toolchain_toml(workspace_dir: Path) -> None:
    toolchain_path = workspace_dir / "ocaml-toolchain.toml"
    if toolchain_path.exists():
        return

    toolchain_path.write_text(
        "\n".join(
            [
                "[toolchain]",
                f'version = "{DEFAULT_TOOLCHAIN_VERSION}"',
                'targets = ["x86_64-unknown-linux-gnu"]',
                "",
            ]
        ),
        encoding="utf-8",
    )


def ensure_workspace_manifest(workspace_root: Path, package_name: str) -> None:
    manifest_path = workspace_root / "riot.toml"
    if manifest_path.exists():
        return

    manifest_path.write_text(
        "\n".join(
            [
                "[workspace]",
                f'members = ["packages/{package_name}"]',
                "",
            ]
        ),
        encoding="utf-8",
    )


def run_command(command: list[str], cwd: Path) -> dict[str, object]:
    started = time.monotonic()
    process = subprocess.run(
        command,
        cwd=str(cwd),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        timeout=900,
        env=command_env(),
    )
    duration_ms = int((time.monotonic() - started) * 1000)
    return {
        "success": process.returncode == 0,
        "exit_code": process.returncode,
        "stdout": process.stdout,
        "stderr": process.stderr,
        "duration_ms": duration_ms,
        "command": command,
    }


def run_command_checked(
    command: list[str],
    cwd: Path,
    failure_prefix: str,
) -> dict[str, object]:
    result = run_command(command, cwd)
    if result["success"]:
        return result

    details = []
    if result["stderr"].strip():
        details.append(result["stderr"].strip())
    if result["stdout"].strip():
        details.append(result["stdout"].strip())
    detail = "\n".join(details)
    if detail:
        raise RuntimeError(f"{failure_prefix}: {detail}")
    raise RuntimeError(f"{failure_prefix}: exit code {result['exit_code']}")


def collect_output_files(root: Path) -> list[dict[str, object]]:
    files: list[dict[str, object]] = []
    for path in sorted(root.rglob("*")):
        if not path.is_file():
            continue

        relative_path = path.relative_to(root).as_posix()
        content_type, _ = mimetypes.guess_type(str(path))
        files.append(
            {
                "path": relative_path,
                "content_base64": base64.b64encode(path.read_bytes()).decode("ascii"),
                "content_type": content_type or "application/octet-stream",
            }
        )
    return files


def read_text_if_exists(path: Path) -> str | None:
    if not path.exists() or not path.is_file():
        return None
    return path.read_text(encoding="utf-8")


def describe_workspace(workspace_root: Path, package_dir: Path) -> str:
    root_manifest = read_text_if_exists(workspace_root / "riot.toml")
    package_manifest = read_text_if_exists(package_dir / "riot.toml")
    root_entries = sorted(path.name for path in workspace_root.iterdir()) if workspace_root.exists() else []
    package_entries = sorted(path.name for path in package_dir.iterdir()) if package_dir.exists() else []
    sections = [
        f"workspace_root={workspace_root}",
        f"workspace_entries={root_entries}",
        f"package_dir={package_dir}",
        f"package_entries={package_entries}",
        f"workspace_riot_toml={(root_manifest or '<missing>').strip()}",
        f"package_riot_toml={(package_manifest or '<missing>').strip()}",
    ]
    return "\n".join(sections)


def command_env() -> dict[str, str]:
    env = os.environ.copy()
    riot_bin_dir = str(RIOT_BIN.parent)
    env["PATH"] = riot_bin_dir + os.pathsep + env.get("PATH", "")
    env.setdefault("HOME", str(Path.home()))
    return env


def download_file(url: str, destination: Path, cwd: Path) -> None:
    run_command_checked(
        [
            "curl",
            "-fsSL",
            "--retry",
            "3",
            "--retry-delay",
            "1",
            "-A",
            "riot-docs-pipeline/1.0",
            "-o",
            str(destination),
            url,
        ],
        cwd,
        f"failed to download {url}",
    )


def ensure_riot_installed(install_url: str) -> None:
    if RIOT_BIN.exists():
        return

    command = f"curl -sSL {install_url} | sh -"
    process = subprocess.run(
        ["sh", "-c", command],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        timeout=900,
        env=command_env(),
    )
    if process.returncode != 0:
        details = process.stderr.strip()
        if process.stdout.strip():
            details = details + ("\n" if details else "") + process.stdout.strip()
        raise RuntimeError(f"failed to install riot from {install_url}: {details}")

    if not RIOT_BIN.exists():
        raise RuntimeError("riot install completed without producing ~/.riot/bin/riot")


def ensure_safe_member(destination_root: Path, member: tarfile.TarInfo) -> None:
    if member.issym() or member.islnk():
        raise ValueError(f"refusing to extract symlink entry {member.name}")

    resolved_target = (destination_root / member.name).resolve()
    resolved_root = destination_root.resolve()
    if os.path.commonpath([str(resolved_root), str(resolved_target)]) != str(resolved_root):
        raise ValueError(f"refusing to extract path outside workspace: {member.name}")


def extract_archive(artifact_path: Path, project_dir: Path) -> None:
    with tarfile.open(artifact_path, "r:gz") as archive:
        members = archive.getmembers()
        for member in members:
            ensure_safe_member(project_dir, member)
        archive.extractall(project_dir, members=members)


def resolve_docs_dir(workspace_dir: Path, package_name: str, package_version: str) -> Path:
    candidates = [
        workspace_dir / "_build" / "doc" / package_name / package_version,
        workspace_dir / "docs" / package_name / package_version,
    ]
    for candidate in candidates:
        if candidate.exists():
            return candidate
    raise FileNotFoundError(
        f"generated docs directory not found for {package_name}@{package_version}"
    )


def process_release(payload: dict[str, object]) -> dict[str, object]:
    package_name = str(payload["package_name"])
    package_version = str(payload["package_version"])
    source_archive_url = str(payload["source_archive_url"])
    riot_install_url = str(payload.get("riot_install_url") or DEFAULT_RIOT_INSTALL_URL)
    generate_docs = bool(payload.get("generate_docs", True))
    verify_build = bool(payload.get("verify_build", True))

    workspace_root = Path(tempfile.mkdtemp(prefix=f"riot_pipeline_{package_name}_"))
    artifact_path = workspace_root / "package.tar.gz"
    project_dir = workspace_root / "workspace"
    package_dir = project_dir / "packages" / package_name
    package_dir.mkdir(parents=True, exist_ok=True)

    try:
        download_file(source_archive_url, artifact_path, workspace_root)
        extract_archive(artifact_path, package_dir)
        ensure_workspace_manifest(project_dir, package_name)
        ensure_toolchain_toml(project_dir)
        ensure_riot_installed(riot_install_url)

        result: dict[str, object] = {}

        if generate_docs:
            docs_result = run_command(
                ["riot", "doc", "--release", "-p", package_name],
                project_dir,
            )
            if not docs_result["success"]:
                diagnostics = describe_workspace(project_dir, package_dir)
                docs_result["stderr"] = f"{docs_result['stderr']}\n\n[workspace diagnostics]\n{diagnostics}".strip()
            if docs_result["success"]:
                docs_dir = resolve_docs_dir(project_dir, package_name, package_version)
                docs_result["output_dir"] = str(docs_dir)
                docs_result["files"] = collect_output_files(docs_dir)
            result["docs"] = docs_result

        if verify_build:
            build_result = run_command(
                ["riot", "build", package_name],
                project_dir,
            )
            if not build_result["success"]:
                diagnostics = describe_workspace(project_dir, package_dir)
                build_result["stderr"] = f"{build_result['stderr']}\n\n[workspace diagnostics]\n{diagnostics}".strip()
            result["build"] = build_result

        return result
    finally:
        shutil.rmtree(workspace_root, ignore_errors=True)


class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/health":
            payload = json_bytes({"ok": True})
            self.send_response(200)
            self.send_header("content-type", "application/json; charset=utf-8")
            self.send_header("content-length", str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)
            return

        self.send_error(404, "Not found")

    def do_POST(self):
        if self.path != "/process":
            self.send_error(404, "Not found")
            return

        content_length = int(self.headers.get("content-length", "0"))
        raw_body = self.rfile.read(content_length)

        try:
            payload = json.loads(raw_body.decode("utf-8"))
            result = process_release(payload)
            body = json_bytes(result)
            self.send_response(200)
            self.send_header("content-type", "application/json; charset=utf-8")
            self.send_header("content-length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        except Exception as error:  # noqa: BLE001
            body = json_bytes(
                {
                    "error": str(error),
                }
            )
            self.send_response(500)
            self.send_header("content-type", "application/json; charset=utf-8")
            self.send_header("content-length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

    def log_message(self, format: str, *args):
        print(
            "%s - - [%s] %s"
            % (
                self.address_string(),
                self.log_date_time_string(),
                format % args,
            )
        )


if __name__ == "__main__":
    server = ThreadingHTTPServer((HOST, PORT), Handler)
    server.serve_forever()
