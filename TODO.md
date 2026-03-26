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
- `--verify-workspace --fail-fast` now passes `1288` files.
- Current first failing file: `packages/syn/tests/fixtures/0400_attribute_item.ml` (syntax-hash mismatch; canonical hashes still diverge).
- Newly fixed in this slice:
  - `packages/syn/src/cst_builder.ml` (previous canonical formatted `format exited 1`)
  - `packages/syn/src/parser.ml` (previous `format exited 1` from parser failing on labeled-arg callsites with polymorphic-variant first labeled argument)
  - typed expressions now always render parenthesized in `krasny`, preventing invalid output like `{...} : t` in expression position
  - polymorphic variant payload parsing in `syn` no longer consumes following labeled arguments (`~...` / `?...`) as payload expressions
  - workspace verification now skips intentionally malformed `syn` diagnostics fixtures under `packages/syn/tests/diagnostics/`
  - new focused fixtures:
    - `0420_typed_expression_parenthesized.ml`
    - `0421_labeled_arg_poly_variant_then_label.ml`
