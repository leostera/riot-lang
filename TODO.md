# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until its completed.
3. Mark a task complete in this document only after the relevant verification has passed.
4. DON'T FORGET TO GIT COMMIT AFTER EVERY SLICE! And use conevntional commit messages like: feat(pkg): <value delivered>

## TASKS

- [ ] Make Krasny able to format the entire codebase without losing information

<!--
these tasks are for later
- [ ] Work on implementing the remaining lints
- [ ] Work on fixing the broken tests
- [ ] Fix `syn`'s fixture mess
-->

### Krasny formats the whole codebase

You are done with this task when we can run: `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast` and there are no failures.

Otherwise, if you find a failure, you will:
1. call `syn print-cst <file>`
2. call `syn print-ceibo <file>`
3. call `krasy syntax-hash <file>`
4. call `krasy format <file>`
5. you will identify the failure and create a small fixture in packages/krasny/tests/fixtures/
6. you will run `./packages/krasny/tests/test_runner.py --filter <new fixture>` to verify it fails
7. you will enter the _fix loop_:
   a. modify krasny or even syn if you need to
   b. if needed, you may write new format_tests or cst_tests
   c. rerun the fixture test runner
   d. once the test passes, you run the --verify-workspace command and see if that file parsed correctly or if you must go on to the next format failure

You are done with this task when `krasny` can format the entire codebase and
the CST-hash of the source before and after formatting is the same (that is, there's no information loss).

Current fail-fast progress (2026-03-26):
- Latest recorded frontier from the external loop log was:
  - `--verify-workspace --fail-fast` passing `1947` files
  - first failure: `packages/syn/tests/fixtures/ocaml_extended_indexoperators.ml` (syntax-hash mismatch after canonical formatting)
- `packages/syn/tests/fixtures/ocaml_extended_indexoperators.ml` now round-trips locally (`orig == fmt` syntax-hash).
- A polling loop script is available at `scripts/verify_fail_fast_loop.sh` and defaults to writing `krasny_verify_results.log` at repo root for live frontier tracking.
- Next workspace frontier is pending a fresh full `--verify-workspace --fail-fast` completion after the fixes below.
- Newly fixed in this slice:
  - `packages/syn/tests/fixtures/ocaml_attributes.ml` (previous `format exited 2` panic in nested module body relift path)
  - `packages/syn/tests/fixtures/ocaml_extended_indexoperators.ml` (previous syntax-hash mismatch from dropped extended index operator punctuation and operator-binding parameter lifting drift)
  - nested module/signature relift now falls back to verbatim source instead of panicking when re-lift fails
  - module/module-type attribute wrappers now render explicitly (`... [@attr]`) instead of dropping attribute shells
  - expression-level attributes are now emitted via `doc_with_expression_attributes`
  - binding headers now preserve parameters for operator-capable binding patterns (`Identifier`/`Operator`) instead of always rewriting to `= fun ... -> ...`
  - new focused fixtures:
    - `0821_nested_module_body_attribute_relift_fallback.ml`
    - `0423_extended_index_operators.ml`
