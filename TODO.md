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
4. Re-run the focused fixture after the build completes.
5. If the new output matches the agreed policy and the fixture was stale, refresh it:
   - `timeout 900 python3 packages/krasny/tests/test_runner.py --filter <fixture_id> --refresh`
6. After a small batch, run broader verification:
   - `timeout 30 tusk test krasny:format_tests`
7. Periodically re-run the whole fixture suite:
   - `timeout 900 python3 packages/krasny/tests/test_runner.py`
8. Commit each coherent slice with a conventional commit.

### Current State

- Structural token cleanup in `syn` is in much better shape now:
  - declaration boundary tokens preserved
  - quantified and mutable tokens preserved
  - recursion booleans moved toward helpers over tokens
- `krasny` has already landed:
  - tighter `:` layout for signatures / annotations / module heads / record type fields
  - typed pattern and expression-ascription layout cleanup
  - improved `fun` / `let` body layout
  - broken apply arguments indented one level deeper than the callee
  - top-level `let rec ... and ...` separated by a blank line
  - better preservation of binding headers

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
   - Lints still need to be updated to the current CST shape.
   - Once `tusk fix` is healthy again, re-enable the `scripts/git-hooks/pre-commit` check so every commit runs:
     - `tusk fix --check`

2. Investigate `tusk fmt` startup latency.
   - It sometimes takes about a second before printing results.
   - Run:
     - `tusk fmt --json`
   - Inspect where the startup time is going and reduce time-to-first-result.

3. Start a new repo-health loop for failing tests.
   - Run `tusk test`
   - Pick one failing test or suite
   - Fix it
   - Re-run
   - Repeat until `tusk test` is green

4. After all of the above is done and committed:
   - start exploring an implementation of `RFD0026`

### Remaining Real Formatter Bugs

- `0343_let_parameter_with_comment_pipeline`
  - inline comment disappears
- `0422_top_level_expression_double_semicolon_before_floating_attribute`
  - `[@@@attr]` downgraded to `[@@attr]`
- `0423_extended_index_operators`
  - index operator declarations/usages mangled
- `0429_qualified_local_open_record_literal`
  - bad terminal `;` / blank line in qualified local-open record literal
- `0430_signature_last_docstring`
  - formatting exits nonzero
- `0431_type_mutual_docstring_between_members`
  - doc ownership still wrong for `type ... and ...`
- `0434_poly_variant_local_open_pattern_payload`
  - extra parens around local-open tuple payload
- `0101_apply_list_trailing_separator`
  - extra space before `]`

### Fixture / Expectation Work

- Full suite status during last pass:
  - `65` passed
  - `98` failed
- After the real bugs above are fixed:
  - refresh stale expectations for the new binding-header / tight-colon / layout policy
  - re-run:
    - `timeout 900 python3 packages/krasny/tests/test_runner.py`
    - `timeout 120 tusk build syn krasny fixme tusk-fix`

### Next Suggested Order

1. Fix `0422` and `0423`
2. Fix `0430` and `0431`
3. Fix `0429` and `0101`
4. Fix `0434`
5. Fix `0343`
6. Refresh stale fixture expectations in controlled batches
