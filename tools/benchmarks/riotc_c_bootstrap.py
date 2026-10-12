#!/usr/bin/env python3
"""Repeatable wall-time/size baseline for Riotc's existing C + Clang bootstrap.

Run from a clean checkout. Build time for the Rust Stage 0 binary is excluded;
each measured sample covers Stage 0 -> JS, JS-hosted C/Clang compilation,
native self-rebuild, and standalone Clang compilation.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import platform
import shlex
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any


def run(command: list[str], *, cwd: Path, log: Path | None = None) -> subprocess.CompletedProcess[str]:
    started = time.perf_counter()
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)
    elapsed = time.perf_counter() - started
    if log is not None:
        log.write_text(
            f"command: {shlex.join(command)}\n"
            f"returncode: {result.returncode}\n"
            f"wall_seconds: {elapsed:.6f}\n\n"
            f"--- stdout ---\n{result.stdout}\n"
            f"--- stderr ---\n{result.stderr}\n",
            encoding="utf-8",
        )
    result.wall_seconds = elapsed  # type: ignore[attr-defined]
    return result


def require_success(result: subprocess.CompletedProcess[str], description: str) -> None:
    if result.returncode != 0:
        raise RuntimeError(f"{description} failed with status {result.returncode}; see its log")


def byte_size(path: Path) -> int:
    return path.stat().st_size


def version(command: list[str], cwd: Path) -> str:
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)
    if result.returncode != 0:
        return f"unavailable (status {result.returncode})"
    return (result.stdout or result.stderr).splitlines()[0]


def git_output(root: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args], cwd=root, text=True, capture_output=True, check=True
    )
    return result.stdout.strip()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--iterations", type=int, default=3, help="sequential samples (default: 3)")
    parser.add_argument("--output-dir", type=Path, help="new directory for logs and artifacts")
    parser.add_argument("--root", type=Path, help="clean checkout to benchmark (default: script's repository)")
    args = parser.parse_args()
    if args.iterations < 1:
        parser.error("--iterations must be at least 1")

    root = (args.root or Path(__file__).resolve().parents[2]).resolve()
    dirty = git_output(root, "status", "--porcelain", "--untracked-files=all")
    if dirty:
        parser.error("run from a clean checkout; git status is not clean")

    if args.output_dir is None:
        output_root = Path(tempfile.mkdtemp(prefix="riotc-c-bootstrap-baseline-"))
    else:
        output_root = args.output_dir.resolve()
        output_root.mkdir(parents=True, exist_ok=False)

    print(f"artifacts: {output_root}")
    build_log = output_root / "stage0-build.log"
    build_result = run(["cargo", "build", "-p", "tinyml"], cwd=root, log=build_log)
    require_success(build_result, "building Stage 0")
    stage0 = root / "target" / "debug" / "tinyml"
    if not stage0.is_file():
        raise RuntimeError(f"Stage 0 binary not found at {stage0}")

    metadata: dict[str, Any] = {
        "timestamp_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
        "revision": git_output(root, "rev-parse", "HEAD"),
        "source_tree_clean": True,
        "host": {
            "platform": platform.platform(),
            "machine": platform.machine(),
            "processor": platform.processor(),
            "cpu_count": os.cpu_count(),
            "python": sys.version.splitlines()[0],
        },
        "tools": {
            "cargo": version(["cargo", "--version"], root),
            "rustc": version(["rustc", "--version"], root),
            "node": version(["node", "--version"], root),
            "clang": version(["clang", "--version"], root),
        },
        "source_fingerprints": {
            path: git_output(root, "rev-parse", f"HEAD:{path}")
            for path in ("riotc/main.ml", "riotc/codegen/c/emit.ml", "riotc/runtime.c", "riotc/runtime.h")
        },
        "stage0_build_seconds_excluded": build_result.wall_seconds,  # type: ignore[attr-defined]
        "output_directory": str(output_root),
        "samples": [],
    }

    for iteration in range(1, args.iterations + 1):
        sample_dir = output_root / f"sample-{iteration:02d}"
        sample_dir.mkdir()
        runner = sample_dir / "run-compiler.mjs"
        runner.write_text(
            'import { main } from "./riotc.mjs";\n'
            "process.exitCode = Number(main(undefined));\n",
            encoding="utf-8",
        )
        source = root / "riotc" / "main.ml"
        native = sample_dir / "riotml-compiler.native"
        successor = sample_dir / "riotml-compiler.successor"

        stage0_result = run(
            [str(stage0), "compile", "-I", str(root / "riotc"), "-o", str(sample_dir / "riotc.mjs"), str(source)],
            cwd=root,
            log=sample_dir / "stage0-to-js.log",
        )
        require_success(stage0_result, "Stage 0 -> JavaScript")

        js_result = run(
            ["node", str(runner), "compile", str(source), "-o", str(native)],
            cwd=root,
            log=sample_dir / "js-to-native.log",
        )
        require_success(js_result, "JS-hosted compiler -> native compiler")

        standalone = sample_dir / "clang-standalone"
        clang_result = run(
            [
                "clang", "-std=gnu11", "-I", str(root / "riotc"),
                str(native) + ".c", str(root / "riotc" / "runtime.c"), "-o", str(standalone),
            ],
            cwd=root,
            log=sample_dir / "standalone-clang.log",
        )
        require_success(clang_result, "standalone Clang compile")

        rebuild_result = run(
            [str(native), "compile", str(source), "-o", str(successor)],
            cwd=root,
            log=sample_dir / "native-self-rebuild.log",
        )
        require_success(rebuild_result, "native compiler self-rebuild")

        smoke_source = sample_dir / "smoke.ml"
        smoke_source.write_text("let main = fn () -> 42\n", encoding="utf-8")
        smoke_executable = sample_dir / "smoke"
        smoke_compile = run(
            [str(successor), "compile", str(smoke_source), "-o", str(smoke_executable)],
            cwd=root,
            log=sample_dir / "smoke-compile.log",
        )
        require_success(smoke_compile, "successor status-42 fixture compile")
        smoke_run = subprocess.run([str(smoke_executable)], cwd=root, check=False)
        if smoke_run.returncode != 42:
            raise RuntimeError(f"status-42 fixture exited {smoke_run.returncode}, expected 42")

        bad_source = sample_dir / "ill-typed.ml"
        bad_source.write_text("let main = fn () -> true\n", encoding="utf-8")
        bad_executable = sample_dir / "must-not-exist"
        bad_compile = run(
            [str(successor), "compile", str(bad_source), "-o", str(bad_executable)],
            cwd=root,
            log=sample_dir / "ill-typed-rejection.log",
        )
        if bad_compile.returncode == 0 or bad_executable.exists() or Path(str(bad_executable) + ".c").exists():
            raise RuntimeError("ill-typed input was accepted or left a plausible artifact")

        metadata["samples"].append(
            {
                "iteration": iteration,
                "wall_seconds": {
                    "stage0_to_js": stage0_result.wall_seconds,  # type: ignore[attr-defined]
                    "js_to_native": js_result.wall_seconds,  # type: ignore[attr-defined]
                    "standalone_clang": clang_result.wall_seconds,  # type: ignore[attr-defined]
                    "native_self_rebuild": rebuild_result.wall_seconds,  # type: ignore[attr-defined]
                    "smoke_compile": smoke_compile.wall_seconds,  # type: ignore[attr-defined]
                },
                "sizes_bytes": {
                    "stage0_javascript": byte_size(sample_dir / "riotc.mjs"),
                    "generated_c_first_build": byte_size(Path(str(native) + ".c")),
                    "native_compiler": byte_size(native),
                    "generated_c_self_rebuild": byte_size(Path(str(successor) + ".c")),
                    "successor_compiler": byte_size(successor),
                    "smoke_c": byte_size(Path(str(smoke_executable) + ".c")),
                    "smoke_executable": byte_size(smoke_executable),
                },
                "smoke_exit_status": smoke_run.returncode,
                "ill_typed_status": bad_compile.returncode,
                "ill_typed_left_no_executable": not bad_executable.exists(),
                "ill_typed_left_no_c_file": not Path(str(bad_executable) + ".c").exists(),
            }
        )
        (output_root / "results.json").write_text(
            json.dumps(metadata, indent=2) + "\n", encoding="utf-8"
        )
        print(f"sample {iteration}/{args.iterations}: {metadata['samples'][-1]}")

    print(f"results: {output_root / 'results.json'}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, subprocess.SubprocessError) as error:
        print(f"benchmark error: {error}", file=sys.stderr)
        raise SystemExit(1)
