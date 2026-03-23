#!/usr/bin/env python3
"""
Round-trip syntax-hash corpus runner for krasny.

Usage:
  python3 roundtrip_runner.py
  python3 roundtrip_runner.py --filter syntax_kind
"""

import argparse
import platform
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import List, Optional

GREEN = "\033[0;32m"
RED = "\033[0;31m"
YELLOW = "\033[0;33m"
NC = "\033[0m"


class RoundtripRunner:
    def __init__(self, workspace_root: Path):
        self.workspace_root = workspace_root
        self.manifest = (
            workspace_root / "packages" / "krasny" / "tests" / "roundtrip_corpus.txt"
        )
        self.binary = self.find_binary()

    def find_binary(self) -> Path:
        direct = self.workspace_root / "_build" / "debug"

        machine = platform.machine().lower()
        system = platform.system().lower()
        arch = {
            "arm64": "aarch64",
            "aarch64": "aarch64",
            "x86_64": "x86_64",
            "amd64": "x86_64",
        }.get(machine, machine)
        os_name = {
            "darwin": "apple-darwin",
            "linux": "unknown-linux-gnu",
        }.get(system)

        if os_name is not None:
            candidate = direct / (arch + "-" + os_name) / "out" / "krasny" / "krasny"
            if candidate.exists():
                return candidate

        candidates = sorted(direct.glob("*/out/krasny/krasny"))
        if candidates:
            return candidates[0]

        fallback = sorted((self.workspace_root / "_build").glob("**/out/krasny/krasny"))
        if fallback:
            return fallback[0]

        return self.workspace_root / "_build" / "debug" / "out" / "krasny" / "krasny"

    def discover_sources(self, filter_pattern: Optional[str] = None) -> List[Path]:
        entries = []
        for line in self.manifest.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if filter_pattern and filter_pattern not in line:
                continue
            entries.append(self.workspace_root / line)
        return entries

    def run_krasny(self, subcommand: str, file_path: Path) -> subprocess.CompletedProcess:
        return subprocess.run(
            [str(self.binary), subcommand, str(file_path)],
            capture_output=True,
            text=True,
            timeout=10.0,
        )

    def run_file(self, file_path: Path) -> bool:
        original_hash = self.run_krasny("syntax-hash", file_path)
        if original_hash.returncode != 0:
            print(f"{RED}✗{NC} {file_path} (syntax-hash exited {original_hash.returncode})")
            if original_hash.stderr:
                print(original_hash.stderr.rstrip())
            return False

        formatted = self.run_krasny("format", file_path)
        if formatted.returncode != 0:
            print(f"{RED}✗{NC} {file_path} (format exited {formatted.returncode})")
            if formatted.stderr:
                print(formatted.stderr.rstrip())
            return False

        suffix = file_path.suffix or ".ml"
        with tempfile.TemporaryDirectory(prefix="krasny-roundtrip-") as tmpdir:
            tmp_path = Path(tmpdir) / ("reformatted" + suffix)
            tmp_path.write_text(formatted.stdout)
            reparsed_hash = self.run_krasny("syntax-hash", tmp_path)
            if reparsed_hash.returncode != 0:
                print(
                    f"{RED}✗{NC} {file_path} "
                    f"(reparse syntax-hash exited {reparsed_hash.returncode})"
                )
                if reparsed_hash.stderr:
                    print(reparsed_hash.stderr.rstrip())
                return False

        if original_hash.stdout == reparsed_hash.stdout:
            print(f"{GREEN}✓{NC} {file_path}")
            return True

        print(f"{RED}✗{NC} {file_path}")
        print(f"  original hash:  {original_hash.stdout.strip()}")
        print(f"  reparsed hash:  {reparsed_hash.stdout.strip()}")
        return False

    def run(self, filter_pattern: Optional[str]) -> int:
        sources = self.discover_sources(filter_pattern)
        if not sources:
            print(f"{YELLOW}No corpus files matched.{NC}")
            return 0

        passed = 0
        failed = 0
        for source in sources:
            if self.run_file(source):
                passed += 1
            else:
                failed += 1

        print()
        if failed == 0:
            print(f"{GREEN}Passed:{NC} {passed}")
            return 0

        print(f"{RED}Failed:{NC} {failed}")
        print(f"{GREEN}Passed:{NC} {passed}")
        return 1


def main() -> int:
    parser = argparse.ArgumentParser(description="krasny round-trip corpus runner")
    parser.add_argument("--filter", help="Run only corpus files whose path contains this string")
    args = parser.parse_args()

    workspace_root = Path(__file__).resolve().parents[3]
    runner = RoundtripRunner(workspace_root)
    return runner.run(args.filter)


if __name__ == "__main__":
    sys.exit(main())
