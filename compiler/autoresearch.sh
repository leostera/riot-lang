#!/usr/bin/env bash
set -euo pipefail

# Honest roadmap metric. Count entries marked with an implementation marker in
# PLAN.md. We only add markers when the corresponding PLAN slice is actually
# implemented and validated. Until then, progress is measured by tests/checks.
completed=$( (grep -E '^<!-- autoresearch:step-[0-9]+:done -->$' PLAN.md 2>/dev/null || true) | wc -l | tr -d ' ')
remaining=$((50 - completed))

checks_passed=0
env LLVM_SYS_221_PREFIX=/opt/homebrew/opt/llvm cargo check --manifest-path stage0/Cargo.toml >/tmp/stage0-check.log 2>&1 && checks_passed=1 || {
  tail -80 /tmp/stage0-check.log >&2
  exit 1
}

printf 'METRIC completed_steps=%s\n' "$completed"
printf 'METRIC remaining_steps=%s\n' "$remaining"
printf 'METRIC checks_passed=%s\n' "$checks_passed"
