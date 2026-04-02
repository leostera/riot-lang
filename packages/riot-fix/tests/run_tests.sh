#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
RUNNER="$ROOT_DIR/packages/riot-fix/tests/test_runner.py"

if [ -n "${1:-}" ]; then
  python3 "$RUNNER" fixtures --filter "$1"
else
  python3 "$RUNNER" fixtures
fi
