# Typ Rewrite Loop

Goal:

- keep `riot run riot -- check -p kernel-new --json` correct
- drive cold-start package checking under `100ms`
- use the OCaml oracle corpus as the correctness rail while we rewrite

Correct currently means:

- `{"files":118,"read_failures":0,"diagnostics":0,"warnings":0}`

## Target Architecture

Build one strong package-check engine:

- one authoritative package-check path
- incremental by module group
- type once, pair once, persist once
- keep authoritative module typings loaded in memory as later groups type
- snapshots stay for query/editor workflows, not package checks

Core checker rules:

- semantic identity uses ids, not strings
- hot indexes use maps/arrays, not linear lists
- no host-side reconstruction pass to recover authoritative module typings
- no query/editor payload on the plain check path
- no compatibility layers kept alive after a refactor lands

When in doubt, match the spirit of `vendor/ocaml/typing`:

- `Ident.t` / `Path.t` style semantic identity
- `typemod` style incremental environment growth
- one authoritative reusable module artifact

## Loop

For every slice:

1. Pick one architectural task that can materially move `kernel-new`.
2. Read:
   - `packages/typ/docs/checker/index.md`
   - `packages/typ/docs/checker/fast_package_check.md`
   - the relevant feature doc under `packages/typ/docs/checker/*`
   - the matching `vendor/ocaml/typing/*` code
3. Write down the OCaml comparison:
   - what OCaml stores once that `typ` still rebuilds
   - what OCaml keys by ids that `typ` still keys by strings
   - what invariant lets OCaml skip work that `typ` still pays for
4. Implement the slice in `packages/typ`.
   Touch `packages/riot-check` or `packages/riot-lsp` only when the new `typ` API requires it.
5. Validate.
6. Measure `kernel-new`.
7. Check oracle coverage on the affected surface.
8. Commit that slice with a conventional commit.
9. Update this file if the loop, benchmark, or architecture target changed.
10. Repeat.

## Validation

Run in this order:

```sh
riot fix ./packages/typ
riot fix ./packages/riot-check
riot fmt ./packages/typ
riot fmt ./packages/riot-check
riot build typ riot-check riot-lsp riot-cli
riot test -p typ
riot test -p riot-lsp
riot run riot -- check -p kernel-new --json
```

Notes:

- `riot fix` may still report existing backlog; do not make it worse.
- if `kernel-new` fails before `typ` runs, fix the consumer path first.
- if behavior changes, inspect the semantic result before approving snapshot churn.

## Oracle

The oracle corpus under `packages/typ/tests/fixtures/oracle` is the correctness rail for the rewrite.

Always run:

```sh
env TYP_ORACLE_SKIP_SNAPSHOT=1 riot test typ:oracle_fixture_tests
```

Use focused runs while developing:

```sh
env TYP_ORACLE_SKIP_SNAPSHOT=1 TYP_ORACLE_START=1 TYP_ORACLE_END=200 riot test typ:oracle_fixture_tests
```

```sh
env TYP_ORACLE_SKIP_SNAPSHOT=1 TYP_ORACLE_FILTER=polyvariant riot test typ:oracle_fixture_tests
```

Keep reporting two numbers:

- corpus coverage: how much of the raw oracle corpus we actively exercise
- typeable surface coverage: how much of the supported language surface is green

If `ocamlc -i` emits an interface that `syn` cannot parse, turn that into a `syn` fixture.

## Performance

Primary benchmark:

```sh
time riot run riot -- check -p kernel-new --json
```

Always compare:

- wall-clock runtime
- emitted `check_summary` timing
- semantic summary
- oracle pass/fail on the touched surface

For event analysis, keep the JSON stream:

```sh
riot run riot -- check -p kernel-new --json > /tmp/kernel-new.jsonl
```

Use the direct built binary when profiling so wrapper overhead does not dominate:

```sh
_build/debug/aarch64-apple-darwin/out/riot-cli/riot check -p kernel-new --json
```

## Xctrace

Use `xctrace` after event timing tells you which phase is hot.

Record:

```sh
xcrun xctrace record \
  --template 'Time Profiler' \
  --time-limit 8s \
  --output /tmp/kernel-new.trace \
  --target-stdout /tmp/kernel-new.jsonl \
  --launch -- \
  _build/debug/aarch64-apple-darwin/out/riot-cli/riot check -p kernel-new --json
```

Export:

```sh
xcrun xctrace export --input /tmp/kernel-new.trace --toc > /tmp/kernel-new-toc.xml
xcrun xctrace export \
  --input /tmp/kernel-new.trace \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
  > /tmp/kernel-new-time-profile.xml
```

Look for:

- repeated module-summary reconstruction
- string/path hashing and structural compare
- list-based lookup or merge churn
- GC/allocation spikes
- retained query payload on check-only paths

Use profiler output to choose the next architectural cut, not to justify micro-tweaks.

## Current Rewrite Priorities

1. finish replacing the old package-check orchestration with one incremental authoritative engine
2. remove remaining string-heavy module identity from the package-check core
3. remove list-heavy graph and merge work from hot paths
4. split check payload from query/editor payload all the way through the package-check engine
5. persist authoritative module typings exactly once, at module completion
6. keep snapshot/query logic separate from the build-check path

## Do Not

- do not optimize before comparing against `vendor/ocaml/typing`
- do not accept string ids or list scans in semantic hot paths
- do not reintroduce dual package-check flows
- do not carry query/editor payload through `riot check`
- do not land compatibility scaffolding and call the rewrite done
- do not trade correctness for speed
- do not commit without recording `kernel-new` timing and oracle status
