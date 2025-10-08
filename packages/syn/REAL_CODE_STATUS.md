# Syn Parser - Real Codebase Status

## Summary

Ran parser on 418 files from the Riot codebase:
- **Passed**: 120 files (29%)
- **Failed**: 298 files (71%)

## Failure Analysis

### Breakdown by Missing Feature

1. **Val declarations** (~100 files)
   - `val name : type`
   - Currently skipped (empty parse tree)
   - **Impact**: Most `.mli` signature files

2. **External declarations** (~50 files)
   - `external name : type = "C_func"`
   - Currently skipped (empty parse tree)  
   - **Impact**: FFI/C bindings

3. **Module system** (~80 files)
   - `module M = struct ... end`
   - `module type S = sig ... end`
   - `include Module`
   - Mixed: some parse, some fail
   - **Impact**: Module definitions and signatures

4. **Advanced type features** (~30 files)
   - `type _ t +=` - Extensible types
   - `type _ t = ...` - GADTs with type indices
   - Produces ERROR/MISSING tokens
   - **Impact**: Advanced type system code

5. **Signature constraints** (~40 files)
   - `module M : S = ...`
   - `include module type of M`
   - Some parse, some fail
   - **Impact**: Module type constraints

## Test Fixtures (9001-9023)

Created 23 real-world test cases:

### Currently Skip (Empty Parse Tree) - 17 tests
These need implementation:
- 9001-9005: Val declarations (simple, arrow, poly, labeled, optional)
- 9006-9008: External declarations (simple, real, multi-name)
- 9009: Module struct
- 9012: Simple include
- 9015: Multiple vals
- 9016-9019: Module features (nested, alias, constraint, functor)
- 9021: Real external declarations
- 9023: Real val declarations

### Currently Fail (ERROR/MISSING) - 6 tests
These need advanced features:
- 9010: Module type signature (`module type S = sig`)
- 9011: Include module type of
- 9013: Extensible types (`type _ t +=`)
- 9014: GADT definition
- 9020: Real include module type of
- 9022: Real extensible type with records

## Recommended Implementation Order

### Phase 1: Basic Declarations (High Impact)
1. **Val declarations** - `val name : type`
   - Would fix ~100 files
   - Straightforward to implement
   
2. **External declarations** - `external name : type = "C_name"`
   - Would fix ~50 files
   - Similar to val, just with C binding

### Phase 2: Module Basics (High Impact)
3. **Module definitions** - `module M = struct ... end`
   - Would fix ~40 files
   - Foundation for module system

4. **Module type signatures** - `module type S = sig ... end`
   - Would fix ~30 files
   - Works with module definitions

5. **Include statements** - `include M`, `include module type of M`
   - Would fix ~20 files
   - Common in interface files

### Phase 3: Advanced (Lower Priority)
6. **GADTs** - `type _ t = | Cons : int -> int t`
   - Would fix ~20 files
   - Complex type system feature

7. **Type extensions** - `type _ t += | Case : ...`
   - Would fix ~10 files
   - Advanced extensible types

## Expected Impact

Implementing Phase 1 + Phase 2:
- Current: 120/418 (29%)
- After Phase 1: ~270/418 (65%)
- After Phase 2: ~350/418 (84%)

This would make syn usable for parsing most real OCaml code!
