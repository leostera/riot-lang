#!/usr/bin/env python3
"""
Real-workspace smoke runner for krasny format.

Usage:
  python3 smoke_runner.py
  python3 smoke_runner.py --filter proc_effect
"""

import argparse
import platform
import subprocess
import sys
from pathlib import Path
from typing import List, Optional

GREEN = "\033[0;32m"
RED = "\033[0;31m"
YELLOW = "\033[0;33m"
NC = "\033[0m"


class SmokeRunner:
    def __init__(self, workspace_root: Path):
        self.workspace_root = workspace_root
        self.manifest = (
            workspace_root / "packages" / "krasny" / "tests" / "smoke_corpus.txt"
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

    def run_krasny(self, file_path: Path) -> subprocess.CompletedProcess:
        return subprocess.run(
            [str(self.binary), "format", str(file_path)],
            capture_output=True,
            text=True,
            timeout=15.0,
        )

    def run_file(self, file_path: Path) -> bool:
        result = self.run_krasny(file_path)
        if result.returncode == 0:
            print(f"{GREEN}✓{NC} {file_path}")
            return True

        print(f"{RED}✗{NC} {file_path} (format exited {result.returncode})")
        if result.stderr:
            print(result.stderr.rstrip())
        return False

    def run(self, filter_pattern: Optional[str]) -> int:
        sources = self.discover_sources(filter_pattern)
        if not sources:
            print(f"{YELLOW}No smoke corpus files matched.{NC}")
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
    parser = argparse.ArgumentParser(description="krasny real-workspace smoke runner")
    parser.add_argument("--filter", help="Run only corpus files whose path contains this string")
    args = parser.parse_args()

    workspace_root = Path(__file__).resolve().parents[3]
    runner = SmokeRunner(workspace_root)
    return runner.run(args.filter)


if __name__ == "__main__":
    sys.exit(main())
