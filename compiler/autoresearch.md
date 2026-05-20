# Autoresearch: Implement compiler/PLAN.md Forward Motion

## Objective
Implement every vertical slice in `compiler/PLAN.md` in order, working on `main` as requested. This is not a performance optimization session; the primary metric is roadmap completion. Every value-adding slice should move forward and be committed. Do not cheat by editing the metric script to count incomplete work.

## Metrics
- **Primary**: `completed_steps` (unitless, higher is better) — number of PLAN entries verifiably implemented.
- **Secondary**: `remaining_steps`, `checks_passed` — progress and correctness monitors.

## How to Run
`./autoresearch.sh` — outputs `METRIC completed_steps=...` lines.

## Files in Scope
- `PLAN.md`: roadmap ledger, only update if implementation status/notes are needed.
- `stage0/**`: Rust bootstrap compiler implementation, fixtures, snapshots.
- `rt/**`: Rust runtime implementation and tests.
- `fixtures/**`: shared compiler fixture programs and expected outputs.
- `autoresearch.md`, `autoresearch.sh`, `autoresearch.checks.sh`, `autoresearch.ideas.md`: loop documentation and harness.

## Off Limits
- Do not alter benchmarks/metrics to claim unimplemented steps.
- Do not revert or reset user work.
- Do not broaden a slice beyond the current PLAN entry unless required by correctness.

## Constraints
- Work directly on `main` per user request.
- Forward motion: prefer `keep` logs and commits for useful progress. Avoid discard/revert workflows.
- Follow `stage0/AGENTS.md` and `rt/AGENTS.md`.
- Each implementation slice should add a positive fixture, diagnostic fixture, or runtime unit test.
- Use validation commands from PLAN for the touched slice when feasible.

## What's Been Tried
- Session initialized on `main`; branch creation was undone by switching back to `main` before implementation.
- Step 1 implemented: removed the one-output-expression restriction for `main`, moved the old two-statement diagnostic into a positive `programs/basic/sequenced_main.ml` fixture with ordered stdout, and validated the fixture suite. Runtime tests can be order-sensitive under parallel execution because of global runtime state; prefer `-- --test-threads=1` for harness checks.
- Step 2 implemented: added `riot_rt_value_eq` structural equality for scalars, strings, tuples, lists, and records; codegen now lowers non-scalar equality through the runtime ABI and preserves scalar LLVM equality. Added nested runtime unit coverage plus stage0 fixtures for string/tuple/list/record equality and a string-vs-i64 diagnostic.
- Step 3 implemented: added `riot_rt_value_lt` for boxed i64/string values, runtime unit coverage, codegen fallback for boxed/static comparisons, positive runtime ordering fixture, and tuple/list/record ordering diagnostics. Direct i64 `<` remains an LLVM signed compare; emitted LLVM for string comparisons calls `riot_rt_value_lt`.
