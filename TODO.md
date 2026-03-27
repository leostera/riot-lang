# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until its completed.
3. Mark a task complete in this document only after the relevant verification has passed.
4. DON'T FORGET TO GIT COMMIT AFTER EVERY SLICE! And use conevntional commit messages like: feat(pkg): <value delivered>

## TASKS

- [ ] Make Krasny able to format the entire codebase without losing information

- [x] `tusk fix --apply` can apply the `packages/std/fix/no_double_list_rev.ml` safely

- [ ] Start building `tusk fmt` to parallely scan the codebase and call
`krasny` on every input -- we can start with a `tusk fmt --check` flag that
will just print out if the input would have been formatted or not, and exists
with 0 if no formatting was needed, or 1 if at least 1 file needed to be
formatted

### Krasny formats the whole codebase

You are done with this task when we can run: `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast` and there are no failures. - A polling loop script is available at `scripts/verify_fail_fast_loop.sh` and defaults to writing `krasny_verify_results.log` at repo root for live frontier tracking.

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

### Parked Release/Toolchain Follow-Up

- Vendored OCaml cross-compiler publication is currently blocked by a relocatability regression after the OCaml 5.5 rebase.
- Current repro on Apple Silicon macOS:
  - `./scripts/release/ocaml.sh aarch64-apple-darwin-x-aarch64-unknown-linux-gnu`
  - fails in `make crossopt` with `Error: Unbound module Stdlib`
- Key evidence gathered:
  - `vendor/ocaml/cross/aarch64-apple-darwin/bin/ocamlc -config` reports `standard_library_default: /usr/local/lib/ocaml`
  - `vendor/ocaml/runtime/build_config.h` currently contains `#define OCAML_STDLIB_DIR "/usr/local/lib/ocaml"`
  - `vendor/ocaml/Makefile` still generates `runtime/build_config.h` from `$(TARGET_LIBDIR)` rather than forcing `../lib/ocaml`
  - manually exporting `OCAMLLIB=$(pwd)/vendor/ocaml/cross/aarch64-apple-darwin/lib/ocaml` makes the native compiler resolve the expected stdlib path, which points at the rebase/build config rather than the release wrapper as the root issue
- Related fallout:
  - `./docker/build.sh` is also currently broken and should be revisited after the relocatable OCaml regression is fixed
  - deploying `services/registry/wrangler.toml` from a Docker image is blocked on the same chain:
    1. fix the broken Docker builder / `./docker/build.sh`
    2. which depends on published prebuilt OCaml toolchains
    3. which depends on restoring the relocatable OCaml fixes after the 5.5 rebase
- When we come back to this, inspect the rebased relocatable patches first before doing more work on the publishing scripts.
