# Typ Performance Loop

Goal:

- keep `riot run riot -- check -p kernel-new --json` correct
- drive cold-start wall-clock time under `100ms`

Correct means the summary stays:

- `{"files":118,"read_failures":0,"diagnostics":0,"warnings":0}`

## Loop

For every slice:

1. Pick one concrete task.
2. Read the relevant `packages/typ/docs/checker/*`.
3. Read the relevant `vendor/ocaml/typing/*`.
4. Ask:
   - what does OCaml store once that `typ` recomputes?
   - what hot path still uses names/paths where OCaml uses ids/descriptors?
   - what structure are we still walking that OCaml skips by invariant?
5. Implement the slice in `packages/typ`, plus `packages/riot-check` /
   `packages/riot-lsp` only when required by the new `typ` API.
6. Run the validation stack.
7. Measure:

   ```sh
   time riot run riot -- check -p kernel-new --json
   ```

8. Compare runtime and semantic summary to the previous checkpoint.
9. Commit only that slice with a conventional commit.
10. Update this file.
11. Repeat.

Slices must stay small enough to measure, understand, and revert independently.

## Validation

Run in this order:

```sh
riot fix ./packages/typ
riot fix ./packages/riot-check
riot fmt ./packages/typ
riot fmt ./packages/riot-check
riot build typ riot-check riot-lsp
riot test -p typ
riot test -p riot-lsp
riot bench -p typ
riot run riot -- check -p kernel-new
```

Notes:

- `riot fix` may still report existing backlog; do not make it worse.
- `riot bench -p typ` currently has no suites; keep running it anyway.
- if `kernel-new` fails before `typ` runs, fix the consumer/planner path first.
- if semantics change, inspect diagnostics and snapshot drift before deciding
  whether the old or new behavior is correct.

## Performance

Primary benchmark:

```sh
time riot run riot -- check -p kernel-new --json
```

Always compare:

- wall-clock runtime
- semantic summary
- `files`
- `read_failures`
- `diagnostics`
- `warnings`

Use the no-deps floors when a slice touches checker internals:

```sh
cd /Users/leostera/Developer/github.com/leostera/riot/packages/riot-check/tests/workspace_fixtures/no_deps_single
time riot check -p solo --json | grep check_summary
```

```sh
cd /Users/leostera/Developer/github.com/leostera/riot/packages/riot-check/tests/workspace_fixtures/no_deps_pair
time riot check -p leaf --json | grep check_summary
```

## Xctrace

Use `xctrace` when runtime events show a hot phase but not the hot code path.

Record a bounded cold-start trace:

```sh
xcrun xctrace record \
  --template 'Time Profiler' \
  --time-limit 8s \
  --output /tmp/kernel-new.trace \
  --target-stdout /tmp/kernel-new.jsonl \
  --launch -- \
  riot run riot -- check -p kernel-new --json
```

Export it:

```sh
xcrun xctrace export --input /tmp/kernel-new.trace --toc > /tmp/kernel-new-toc.xml
xcrun xctrace export \
  --input /tmp/kernel-new.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
  > /tmp/kernel-new-time-profile.xml
```

Look for:

- GC / allocation-heavy stacks
- structural compare (`compare_val`, polymorphic equality/compare)
- list-based lookups (`List.mem`, `List.assoc_opt`, etc.)
- repeated parser / CST work
- time after `typ_prepare_snapshot_finish` in `riot-check`

Use `xctrace` after event-level timing, not instead of comparing against
`vendor/ocaml/typing/*`.

## Current Items

- [ ] remove the remaining pre-first-pair work in `Session.prepare_snapshot`
- [ ] flatten snapshot/shared cache identities everywhere
- [ ] cache the fully built initial infer env per semantic input
- [ ] finish stopping rebuilds / requalification of ambient exports and type decls
- [x] cache merged initial envs by ambient-surface identity instead of source id
- [ ] cut `List.mem` / assoc / remove-assoc hot paths out of session checking
- [ ] remove remaining solver-time / checker-time name resolution work

## Checkpoints

For each clean slice, record:

- slice name
- commit hash
- `kernel-new` runtime
- semantic summary
- optional no-deps floors if checker internals changed

Current attempted slice:

- `Session.prepare_snapshot` deferred nested-module prefix discovery until direct local/loaded resolution leaves unresolved module names
- commit hash: pending (`packages/typ/src/session/Session.ml` already had unrelated in-flight changes in this worktree)
- `kernel-new` runtime: `real 8.52s` from `/usr/bin/time -p` under contention from a stuck `riot test -p typ` `session_tests` process; live JSON event timing moved total check time from `7368ms` to `6792ms` and `typ_prepare_snapshot` from `4822ms` to `282ms`
- semantic summary: `{"files":118,"read_failures":0,"diagnostics":0,"warnings":0}`
- no-deps single floor: `real 0.06s`, unchanged from `real 0.06s`
- no-deps pair floor: `real 0.04s`, down from `real 0.05s`
- notes: `riot run riot -- check -p kernel-new --json` kept the summary stable and moved the first `typ_module_pairing_start` to after `typ_prepare_snapshot_finish`; `riot build typ riot-check riot-lsp`, `riot test -p riot-lsp`, `riot bench -p typ`, and `riot run riot -- check -p kernel-new` passed; `riot test -p typ` still reported the existing snapshot drift from the current prelude/bootstrap worktree state and then remained busy in `session_tests` for >20 minutes, so the wall-clock benchmark should be treated as contended

Latest measured slice:

- `Snapshot` keyed local ambient + initial infer env caches by ambient surface
- commit hash: pending (`packages/typ/src/session/Snapshot.ml` already had unrelated in-flight changes in this worktree)
- `kernel-new` runtime: `real 6.05s` after the slice, down from `real 10.66s`
- semantic summary: `{"files":118,"read_failures":0,"diagnostics":0,"warnings":0}`
- no-deps single floor: `real 0.06s`, up from `real 0.05s`
- no-deps pair floor: `real 0.05s`, unchanged from `real 0.05s`
- notes: `riot test -p typ` still reports existing snapshot drift from the current prelude/bootstrap worktree state, but `riot build typ riot-check riot-lsp`, `riot test -p riot-lsp`, `riot bench -p typ`, `riot run riot -- check -p kernel-new`, and the live JSON summary all stayed semantically stable

Previous measured slice:

- `Snapshot` loaded ambient module surfaces cache
- commit hash: pending (`packages/typ/src/infer/checker.ml`, `packages/typ/src/infer/checker.mli`, and `packages/typ/src/session/Snapshot.ml` already had unrelated in-flight changes)
- `kernel-new` runtime: `real 10.66s` after the slice, down from `real 13.35s`
- semantic summary: `{"files":118,"read_failures":0,"diagnostics":0,"warnings":0}`
- no-deps single floor: `real 0.05s`, down from `real 0.08s`
- no-deps pair floor: `real 0.05s`, down from `real 0.10s`
- notes: `riot test -p typ` still has existing snapshot drift from the current prelude/bootstrap worktree state, but `riot build typ riot-check riot-lsp`, `riot test -p riot-lsp`, and the live `kernel-new` check all stayed semantically stable

## Heuristics

- prefer OCaml algorithmic parity over guessed micro-optimizations
- move env work toward stored symbolic tables, not repeated flattening
- move descriptor work toward ids, not names or paths
- move solver work toward levels, pools, and local copy scopes
- correctness fixes still need benchmarking

## Do Not

- do not optimize without checking the relevant OCaml typing code first
- do not treat profiler output as a substitute for algorithmic comparison
- do not land giant rewrites without checkpoints
- do not keep semantically neutral slowdowns
- do not ignore semantic-summary drift when runtime improves
- do not let strings / `IdentPath` leak deeper into hot paths
- do not reintroduce flattened env reconstruction in lookup paths
- do not mix unrelated work into one performance slice
- do not commit a slice without recording its runtime
