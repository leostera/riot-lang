# tusk-fix test suite

This directory contains end-to-end fixtures for `tusk fix` plus OCaml unit tests.

## Structure

- `000*_*.ml`: fixture inputs
- `*.ml.expected`: pretty-printed snapshots aggregated from `tusk fix --check --json` JSONL events
- `test_runner.py`: fixture runner modeled after `syn`'s runner
- `run_tests.sh`: shell wrapper around the Python runner
- `regenerate_expected.sh`: refresh fixture snapshots
- `lint_codebase.sh`: run `tusk fix` over the repo and report files with issues

## Running tests

```bash
python3 packages/tusk-fix/tests/test_runner.py fixtures
python3 packages/tusk-fix/tests/test_runner.py fixtures --filter 0001
python3 packages/tusk-fix/tests/test_runner.py all
```

## Refreshing expected output

```bash
python3 packages/tusk-fix/tests/test_runner.py fixtures --refresh
python3 packages/tusk-fix/tests/test_runner.py fixtures --refresh --filter 0001
```

## Codebase audit

```bash
python3 packages/tusk-fix/tests/test_runner.py codebase
```
