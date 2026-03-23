#!/usr/bin/env python3
"""
Expectation runner for formatter-specific krasny fixtures.

Usage:
  python3 format_runner.py
  python3 format_runner.py --filter 0117
  python3 format_runner.py --refresh
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


class FormatRunner:
    def __init__(self, workspace_root: Path):
        self.workspace_root = workspace_root
        self.fixtures_dir = workspace_root / "packages" / "krasny" / "tests" / "fixtures"
        self.manifest = (
            workspace_root / "packages" / "krasny" / "tests" / "format_expectations.txt"
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
        sources: List[Path] = []
        for line in self.manifest.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if filter_pattern and filter_pattern not in line:
                continue
            sources.append(self.fixtures_dir / line)
        return sources

    def run_krasny(self, file_path: Path) -> subprocess.CompletedProcess:
        return subprocess.run(
            [str(self.binary), "format", str(file_path)],
            capture_output=True,
            text=True,
            timeout=10.0,
        )

    def run_fixture(self, file_path: Path, refresh: bool) -> bool:
        expected_path = Path(str(file_path) + ".expected")
        result = self.run_krasny(file_path)
        if result.returncode != 0:
            print(f"{RED}✗{NC} {file_path} (formatter exited {result.returncode})")
            if result.stderr:
                print(result.stderr.rstrip())
            return False

        actual = result.stdout

        if refresh or not expected_path.exists():
            expected_path.write_text(actual)
            print(f"{YELLOW}↺{NC} {file_path} (refreshed)")
            return True

        expected = expected_path.read_text()
        if actual == expected:
            print(f"{GREEN}✓{NC} {file_path}")
            return True

        print(f"{RED}✗{NC} {file_path}")
        return False

    def run(self, filter_pattern: Optional[str], refresh: bool) -> int:
        sources = self.discover_sources(filter_pattern)
        if not sources:
            print(f"{YELLOW}No format fixtures matched.{NC}")
            return 0

        passed = 0
        failed = 0
        for source in sources:
            if self.run_fixture(source, refresh):
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
    parser = argparse.ArgumentParser(description="krasny formatter expectation runner")
    parser.add_argument("--filter", help="Run only fixtures whose name contains this string")
    parser.add_argument("--refresh", action="store_true", help="Rewrite expectations from current formatter output")
    args = parser.parse_args()

    workspace_root = Path(__file__).resolve().parents[3]
    runner = FormatRunner(workspace_root)
    return runner.run(args.filter, args.refresh)


if __name__ == "__main__":
    sys.exit(main())
