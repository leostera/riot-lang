#!/usr/bin/env python3
"""
Fixture runner for krasny.

Every fixture in the manifest is validated in two ways:
  1. formatter output matches the adjacent .expected file
  2. the formatted output round-trips its CST syntax hash

Usage:
  python3 test_runner.py
  python3 test_runner.py --filter 0117
  python3 test_runner.py --refresh
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


class RunnerContext:
    def __init__(self, workspace_root: Path):
        self.workspace_root = workspace_root
        self.tests_dir = workspace_root / "packages" / "krasny" / "tests"
        self.fixtures_dir = self.tests_dir / "fixtures"
        self.manifest = self.tests_dir / "format_expectations.txt"
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

    def run_krasny(
        self,
        subcommand: str,
        file_path: Path,
        *,
        timeout_seconds: float,
    ) -> subprocess.CompletedProcess:
        return subprocess.run(
            [str(self.binary), subcommand, str(file_path)],
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
        )


class FixtureRunner:
    def __init__(self, context: RunnerContext):
        self.context = context

    def discover_fixtures(self, filter_pattern: Optional[str]) -> List[Path]:
        fixtures = []
        for line in self.context.manifest.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if filter_pattern and filter_pattern not in line:
                continue
            fixtures.append(self.context.fixtures_dir / line)
        return fixtures

    def check_expectation(
        self,
        fixture_path: Path,
        actual: str,
        *,
        refresh: bool,
    ) -> tuple[bool, str]:
        expected_path = Path(str(fixture_path) + ".expected")

        if refresh or not expected_path.exists():
            expected_path.write_text(actual)
            return True, "refreshed expectation"

        expected = expected_path.read_text()
        if actual == expected:
            return True, "expectation ok"

        return False, f"expected output mismatch in {expected_path.name}"

    def check_roundtrip(self, fixture_path: Path, actual: str) -> tuple[bool, str]:
        suffix = fixture_path.suffix or ".ml"
        with tempfile.TemporaryDirectory(prefix="krasny-roundtrip-") as tmpdir:
            tmp_path = Path(tmpdir) / ("formatted" + suffix)
            tmp_path.write_text(actual)

            original_hash = self.context.run_krasny(
                "syntax-hash",
                tmp_path,
                timeout_seconds=10.0,
            )
            if original_hash.returncode != 0:
                detail = f"syntax-hash exited {original_hash.returncode}"
                if original_hash.stderr:
                    detail += f": {original_hash.stderr.rstrip()}"
                return False, detail

            reformatted = self.context.run_krasny(
                "format",
                tmp_path,
                timeout_seconds=10.0,
            )
            if reformatted.returncode != 0:
                detail = f"format exited {reformatted.returncode}"
                if reformatted.stderr:
                    detail += f": {reformatted.stderr.rstrip()}"
                return False, detail

            reformatted_path = Path(tmpdir) / ("reformatted" + suffix)
            reformatted_path.write_text(reformatted.stdout)
            reparsed_hash = self.context.run_krasny(
                "syntax-hash",
                reformatted_path,
                timeout_seconds=10.0,
            )

        if reparsed_hash.returncode != 0:
            detail = f"reparse syntax-hash exited {reparsed_hash.returncode}"
            if reparsed_hash.stderr:
                detail += f": {reparsed_hash.stderr.rstrip()}"
            return False, detail

        if original_hash.stdout == reparsed_hash.stdout:
            return True, "roundtrip ok"

        detail = "\n".join(
            [
                "roundtrip syntax hash mismatch",
                f"  original hash: {original_hash.stdout.strip()}",
                f"  formatted hash: {reparsed_hash.stdout.strip()}",
            ]
        )
        return False, detail

    def run_fixture(self, fixture_path: Path, *, refresh: bool) -> bool:
        format_result = self.context.run_krasny(
            "format",
            fixture_path,
            timeout_seconds=10.0,
        )
        if format_result.returncode != 0:
            print(f"{RED}✗{NC} {fixture_path}")
            print(f"  format exited {format_result.returncode}")
            if format_result.stderr:
                print(format_result.stderr.rstrip())
            return False

        actual = format_result.stdout
        expectation_ok, expectation_detail = self.check_expectation(
            fixture_path,
            actual,
            refresh=refresh,
        )
        roundtrip_ok, roundtrip_detail = self.check_roundtrip(fixture_path, actual)

        if expectation_ok and roundtrip_ok:
            marker = YELLOW + "↺" + NC if refresh else GREEN + "✓" + NC
            print(f"{marker} {fixture_path}")
            return True

        print(f"{RED}✗{NC} {fixture_path}")
        if not expectation_ok:
            print(f"  {expectation_detail}")
        if not roundtrip_ok:
            print(f"  {roundtrip_detail}")
        return False

    def run(self, filter_pattern: Optional[str], *, refresh: bool) -> int:
        fixtures = self.discover_fixtures(filter_pattern)
        if not fixtures:
            print(f"{YELLOW}No fixtures matched.{NC}")
            return 0

        passed = 0
        failed = 0
        for fixture in fixtures:
            if self.run_fixture(fixture, refresh=refresh):
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


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Run krasny fixture expectations and syntax-hash roundtrips",
    )
    parser.add_argument(
        "--filter",
        help="Run only fixtures whose manifest path contains this string",
    )
    parser.add_argument(
        "--refresh",
        action="store_true",
        help="Rewrite .expected files from current formatter output",
    )
    args = parser.parse_args(argv)

    workspace_root = Path(__file__).resolve().parents[3]
    context = RunnerContext(workspace_root)
    runner = FixtureRunner(context)
    return runner.run(args.filter, refresh=args.refresh)


if __name__ == "__main__":
    sys.exit(main())
