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
- Step 4 implemented: parsed lowercase dotted postfix as `Field` instead of path segments while preserving uppercase module paths, carried field access through typed HIR and RIR, added `riot_rt_value_record_get`, and lowered field projection through runtime codegen. Added `runtime_record_field.ml` to prove a record returned from a function is projected at runtime; `emit ir` contains `Field` nodes.
- Step 5 implemented: numeric dotted postfix like `tuple.0` now parses as `TupleIndex`, flows through typed HIR/RIR, lowers to `riot_rt_value_tuple_get`, and has runtime tuple_get coverage. Added `tuple_projection.ml` covering literals, locals, function returns, and nested tuples, plus an out-of-bounds diagnostic for statically-known tuple literals. Note: chained tuple numeric projections currently need parentheses (`(nested().0).1`) because `.0.1` is lexed ambiguously as a float-like token.
- Step 6 implemented: added source-level builtins `list_len(xs)` and `list_get(xs, i)` that lower to `riot_rt_value_list_len` and `riot_rt_value_list_get`. Runtime tests cover list length/get through the compound-values unit test; `list_operations.ml` covers empty/non-empty length and scalar/boxed indexing. LLVM output shows direct runtime ABI calls.
- Step 7 implemented: added source-level builtins `string_len(s)` and `string_concat(a, b)` that lower to `riot_rt_value_string_len` and `riot_rt_value_string_concat`. Runtime compound-value test covers length/concat; `string_operations.ml` covers literal concat, variable concat, nested concat, and length. LLVM output calls runtime string ABI rather than embedding final concatenated strings.
