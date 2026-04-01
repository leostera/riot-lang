# TODO

## Krasny

### Loop

1. Pick one real formatter regression first, not expectation drift.
2. Reproduce it with a focused fixture run:
   - `timeout 900 python3 packages/krasny/tests/test_runner.py --filter <fixture_id>`
3. If the fix changes `krasny` code, rebuild before trusting fixture output:
   - `timeout 120 tusk build krasny`
   - or, when `syn`/shared fallout is possible:
     `timeout 120 tusk build syn krasny fixme tusk-fix`
4. If the fix changes `syn` parser/CST behavior, also run the focused `syn` fixture coverage:
   - `timeout 900 python3 packages/syn/tests/test_runner.py fixtures --filter <pattern>`
   - `timeout 900 python3 packages/syn/tests/test_runner.py cst --filter <pattern>`
   - use `--refresh-clean` when the new parse/CST output is correct and stale expectations are the only failure
5. Re-run the focused `krasny` fixture after the build completes.
6. If the new output matches the agreed policy and the fixture was stale, refresh it:
   - `timeout 900 python3 packages/krasny/tests/test_runner.py --filter <fixture_id> --refresh`
7. After a small batch, run broader verification:
   - `timeout 30 tusk test krasny:format_tests`
8. Periodically re-run the whole fixture suite:
   - `timeout 900 python3 packages/krasny/tests/test_runner.py`
9. Commit each coherent slice with a conventional commit.

### Current State

- Syntax/layout stabilization is green:
  - `timeout 900 python3 packages/krasny/tests/test_runner.py`
  - `timeout 900 python3 packages/syn/tests/test_runner.py fixtures`
  - `timeout 900 python3 packages/syn/tests/test_runner.py cst`
  - `timeout 30 tusk test krasny:format_tests`
  - `timeout 180 tusk test syn:cst_tests`
- Structural token cleanup in `syn` is in much better shape now:
  - declaration boundary tokens preserved
  - quantified and mutable tokens preserved
  - recursion booleans moved toward helpers over tokens
  - `#` method calls only lift as method calls when followed by identifier-like tokens
  - coercions now preserve the original `:>` token sequence instead of looking for a fake combined token
- `krasny` has already landed:
  - tighter `:` layout for signatures / annotations / module heads / record type fields
  - typed pattern and expression-ascription layout cleanup
  - improved `fun` / `let` body layout
  - broken apply arguments indented one level deeper than the callee
  - top-level `let rec ... and ...` separated by a blank line
  - better preservation of binding headers
  - full fixture suite currently green

### Layout Policy We Agreed On

- `:` is tight:
  - `val create: ...`
  - `module A: sig ... end`
  - `field: type`
  - `method render: env -> node`
- `=` is spaced:
  - `type t = ...`
  - `field = value`
- If the rhs fits, keep it inline.
- If it does not fit, break immediately after the separator.
- Expression ascriptions/coercions stay parenthesized:
  - `let x = (expr: t)`
  - multiline:
    ```ocaml
    let x = (
      expr:
        very_long_type
    )
    ```
- Prefix docs align with the declaration/member/constructor/field they describe.
- Postfix constructor docs are unsupported and normalize as leading trivia for the next item.

### Important Follow-Up

- We 100% want to deliberately desugar `let` functions later.
  - Keep that as an explicit formatter project after the current layout/fixture stabilization work.

### After Syntax/Layout Stabilization

1. Fix `tusk fix` for the new CST.
   - Generated `fixme-runner` now builds against the current CST again.
   - `timeout 180 tusk test tusk-fix:runner_tests` is green.
   - Current blocker: `timeout 120 tusk fix --check` now scans until timeout instead of failing at compile time.
   - Once `tusk fix` is healthy again, re-enable the `scripts/git-hooks/pre-commit` check so every commit runs:
     - `tusk fix --check`
   - Loop:
     - `timeout 120 tusk build syn fixme tusk-fix`
      - `timeout 120 tusk fix --check`
     - fix one root CST consumer at a time
     - re-run `timeout 180 tusk test syn:cst_tests`
     - re-run `timeout 30 tusk test krasny:format_tests` when formatter-facing CST APIs change

2. Investigate `tusk fmt` startup latency.
   - It sometimes takes about a second before printing results.
   - Run:
     - `tusk fmt --json`
   - Inspect where the startup time is going and reduce time-to-first-result.
   - Verification loop:
     - capture baseline `tusk fmt --json`
     - make one startup-path change
     - rerun `tusk fmt --json`
     - compare time-to-first-result and total runtime

3. Start a new repo-health loop for failing tests.
   - Run `tusk test`
   - Pick one failing test or suite
   - Fix it
   - Re-run
   - Repeat until `tusk test` is green

4. After all of the above is done and committed:
   - start exploring an implementation of `RFD0026`
