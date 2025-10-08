# Syn Parser Test Suite - Quick Start

## Quick Stats
- **Total tests**: 1050
- **New tests**: 250 (0801-1050)
- **All validated**: ✅ Valid OCaml syntax

## 5 Phases of New Tests

```
Phase 1: Type Expressions     [0801-0850]  50 tests  ✅
Phase 2: Type Definitions     [0851-0930]  80 tests  ✅
Phase 3: Pattern Features     [0931-0970]  40 tests  ✅
Phase 4: Expression Features  [0971-1000]  30 tests  ✅
Phase 5: Module System        [1001-1050]  50 tests  ✅
```

## Run Tests

```bash
# All tests
./packages/syn/tests/run_tests.sh

# Phase 1 only
./packages/syn/tests/run_tests.sh 2>&1 | grep -E "08[0-5][0-9]"

# Single test
tusk run syn -- parse ./packages/syn/tests/fixtures/0801_type_var_single.ml
```

## Implementation Order

1. **Start here**: Phase 1 (Type Expressions) - 50 tests
   - Most fundamental
   - Required for everything else
   - Tests: 0801-0850

2. **Then**: Phase 2 (Type Definitions) - 80 tests
   - Builds on Phase 1
   - Adds `type` declarations
   - Tests: 0851-0930

3. **Next**: Phase 3 (Patterns) - 40 tests
   - Or patterns, as patterns, typed patterns
   - Tests: 0931-0970

4. **Then**: Phase 4 (Expressions) - 30 tests
   - Type annotations, assignments
   - Tests: 0971-1000

5. **Finally**: Phase 5 (Modules) - 50 tests
   - Module system and functors
   - Tests: 1001-1050

## Sample Test

**Input** (`0801_type_var_single.ml`):
```ocaml
type 'a t = 'a
```

**Expected Output** (when implemented):
```json
{
  "tree": {
    "type": "node",
    "kind": "SOURCE_FILE",
    "children": [...]
  },
  "diagnostics": []
}
```

## Generate Expected Output

After implementing a feature:
```bash
tusk run syn -- parse --json file.ml > file.ml.expected
```

## Test Success = No ERROR/MISSING Nodes

❌ **Bad** (not implemented):
```json
{
  "tree": {
    "children": [
      {"kind": "ERROR", ...}
    ]
  },
  "diagnostics": [{"kind": "UnexpectedToken", ...}]
}
```

✅ **Good** (implemented correctly):
```json
{
  "tree": {
    "children": [
      {"kind": "TYPE_DECL", ...}
    ]
  },
  "diagnostics": []
}
```

## Documentation

- **Comprehensive Guide**: `./TEST_SUITE_GUIDE.md`
- **Summary**: `./NEW_TESTS_SUMMARY.md`
- **This File**: Quick reference

## Key Commands

```bash
# Format/validate all tests
tusk fmt

# Build parser
tusk build -p syn

# Run parser on file
tusk run syn -- parse <file>

# Run all tests
./packages/syn/tests/run_tests.sh
```

## What Each Phase Tests

| Phase | Focus | Example |
|-------|-------|---------|
| 1 | Type syntax | `type 'a f = 'a -> 'a` |
| 2 | Type declarations | `type t = A \| B of int` |
| 3 | Pattern matching | `\| x :: xs as list -> ...` |
| 4 | Type annotations | `let x = (42 : int)` |
| 5 | Modules | `module M = struct ... end` |

## Get Started

1. Read `TEST_SUITE_GUIDE.md` for full details
2. Start implementing Phase 1 (type expressions)
3. Run tests frequently
4. Generate expected outputs as you go
5. Move to next phase

Good luck! 🚀
