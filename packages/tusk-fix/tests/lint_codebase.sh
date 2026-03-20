#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
RUNNER="$ROOT_DIR/packages/tusk-fix/tests/test_runner.py"

python3 "$RUNNER" codebase
