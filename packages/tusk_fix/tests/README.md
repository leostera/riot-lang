# tusk_fix Test Suite

This directory contains tests for the tusk_fix linter.

## Structure

- `*.ml` - Test input files
- `*.ml.expected` - Expected linter output for each test
- `run_tests.sh` - Run all tests and report pass/fail
- `regenerate_expected.sh` - Regenerate expected outputs (use after fixing bugs or changing output format)

## Running Tests

```bash
# Run all tests
./packages/tusk_fix/tests/run_tests.sh

# Run specific test by number
./packages/tusk_fix/tests/run_tests.sh 0001

# Run tests matching pattern
./packages/tusk_fix/tests/run_tests.sh nostdlib
```

## Regenerating Expected Outputs

After fixing a bug or intentionally changing output format:

```bash
# Regenerate all
./packages/tusk_fix/tests/regenerate_expected.sh

# Regenerate specific test
./packages/tusk_fix/tests/regenerate_expected.sh 0001
```

## Test Cases

### 0001_nostdlib_open.ml
Tests detection of stdlib modules in `open` statements.
- `open Unix`
- `open Hashtbl`

### 0002_nostdlib_module_path.ml
Tests detection of stdlib modules in module paths (e.g., `Unix.getenv`).
**Status**: Not yet implemented

### 0003_nostdlib_type_annotation.ml
Tests detection of stdlib modules in type annotations (e.g., `Queue.t`).
**Status**: Not yet implemented

### 0004_nostdlib_mixed.ml
Tests mixed usage of stdlib modules.
**Status**: Partially implemented (only open statements detected)

### 0005_nostdlib_clean.ml
Tests that clean code produces no warnings.
**Status**: Working

## Adding New Tests

1. Create a new test file: `NNNN_description.ml`
2. Run `./packages/tusk_fix/tests/regenerate_expected.sh NNNN` to generate expected output
3. Review the expected output to ensure it's correct
4. Run `./packages/tusk_fix/tests/run_tests.sh NNNN` to verify the test passes
