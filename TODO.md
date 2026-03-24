# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until its completed.
3. Mark a task complete in this document only after the relevant verification has passed.
4. DON'T FORGET TO GIT COMMIT AFTER EVERY SLICE! And use conevntional commit messages like: feat(pkg): <value delivered>

## TASKS

- [ ] Work on Krasny
- [ ] Work on implementing the remaining lints
- [ ] Work on fixing the broken tests

## Verification Commands

1. Rebuild `tusk` when build-system, parser, or lint-runtime changes affect the binary:
   - `tusk build <pkg>`
3. After a package slice lands, rerun the focused test suites:
   - `tusk test <pkg>:cst_tests`
4. If the package has an expectation test runner, run it:
   - `timeout 900 python3 packages/<pkg>/tests/test_runner.py`

## krasny Checklist

Rough guidelines for formatting decisions:
* optimize for readability, newlines are cheap
* ~100 columns is good, we have wider screens now
* put comments _before_ the item they document! not after. double check the ast parses them well!
* on type definitions and whenever you can line-break, you should!
  for ex. type a = | A | B | C should be formattes as
          type a =
                 | A 
                 | B
                 | C
  same for match arms, put them one in each line
* remove parenthesis wherever possible
* and format large numbers with _s by default: 1000 -> 1_000, 10022 -> 10_022 

### Bootstrap and Safety

- [x] Initialize the `krasny` package, CLI, focused tests, and expectation harness
- [x] Seed `krasny` formatter fixtures from `packages/syn/tests/fixtures`
- [x] Add a focused round-trip syntax-hash invariant test for selected real codebase files
- [x] Add a dedicated round-trip syntax-hash corpus runner over selected repo files
- current green corpus: `11` repo files across `krasny`, `syn`, `std`, and `tusk-fix`
- [x] Add a `krasny` expectation suite for formatted output, separate from the current lossless-token baseline
- bootstrap status: `krasny` builds, focused tests pass, the format expectation runner is green on the first curated block (`90` fixtures), and the round-trip syntax-hash corpus is green on `11` selected repo files

### Formatter Pipeline

- [x] Replace the current lossless token renderer with a CST-driven `Doc` lowering pipeline
- [x] Introduce the `Doc` / layout engine used by both `krasny format` and future synthetic fix rendering
- [ ] Add comment and trivia attachment rules to the formatter pipeline
  - first slice landed for leading top-level comments/docstrings between supported implementation items
  - second slice landed for preserving verbatim unsupported top-level `let` bindings between formatted items
  - third slice landed for preserving verbatim unsupported top-level non-`let` items while only rewriting adjacent mixed-file `let` bindings when their binding text stays stable
- [x] Require a successful CST lift before formatting; do not pretty-print broken files
- [ ] Add a codebase smoke runner for `krasny format` over real workspace files

### Trivia

- [ ] `WHITESPACE`
- [ ] `COMMENT`
- [ ] `DOCSTRING`

### Literals

- [x] `INT_LITERAL`
- [x] `FLOAT_LITERAL`
- [x] `STRING_LITERAL`
- [x] `CHAR_LITERAL`
- [x] `BOOL_LITERAL`
- [x] `UNIT_LITERAL`

### Expressions

- [x] `IDENT_EXPR`
- [x] `PATH_EXPR`
- [x] `APPLY_EXPR`
- [x] `LABELED_ARG`
- [x] `OPTIONAL_ARG`
- [x] `INFIX_EXPR`
- [x] `PREFIX_EXPR`
- [x] `IF_EXPR`
- [x] `MATCH_EXPR`
- [x] `FUN_EXPR`
- [x] `LABELED_PARAM`
- [x] `OPTIONAL_PARAM`
- [x] `OPTIONAL_PARAM_DEFAULT`
- [x] `FUNCTION_EXPR`
- [x] `LET_EXPR`
- [x] `LET_REC_EXPR`
- [ ] `SEQUENCE_EXPR`
- [x] `PAREN_EXPR`
- [ ] `TUPLE_EXPR`
- [ ] `LIST_EXPR`
- [ ] `ARRAY_EXPR`
- [ ] `RECORD_EXPR`
- [ ] `RECORD_UPDATE_EXPR`
- [ ] `UNREACHABLE_EXPR`
- [ ] `FIELD_ACCESS_EXPR`
- [ ] `ARRAY_INDEX_EXPR`
- [ ] `STRING_INDEX_EXPR`
- [ ] `ASSIGN_EXPR`
- [ ] `CONSTRUCTOR_EXPR`
- [ ] `POLY_VARIANT_EXPR`
- [ ] `ASSERT_EXPR`
- [ ] `LAZY_EXPR`
- [ ] `WHILE_EXPR`
- [ ] `FOR_EXPR`
- [ ] `TRY_EXPR`
- [ ] `TYPED_EXPR`
- [ ] `COERCE_EXPR`
- [ ] `ATTRIBUTE_EXPR`
- [ ] `EXTENSION_EXPR`
- [ ] `OBJECT_EXPR`
- [ ] `OBJECT_SELF`
- [ ] `OBJECT_METHOD`
- [ ] `OBJECT_VAL`
- [ ] `OBJECT_INHERIT`
- [ ] `OBJECT_UPDATE_EXPR`
- [ ] `METHOD_CALL_EXPR`
- [ ] `NEW_EXPR`
- [ ] `LOCAL_OPEN_EXPR`
- [ ] `LET_MODULE_EXPR`
- [ ] `FIRST_CLASS_MODULE_EXPR`
- [ ] `STRUCT_EXPR`
- [ ] `MODULE_PATH`

### Patterns

- [x] `IDENT_PATTERN`
- [x] `WILDCARD_PATTERN`
- [x] `LITERAL_PATTERN`
- [x] `CONSTRUCTOR_PATTERN`
- [x] `TUPLE_PATTERN`
- [x] `LIST_PATTERN`
- [ ] `ARRAY_PATTERN`
- [ ] `CONS_PATTERN`
- [ ] `RECORD_PATTERN`
- [x] `OR_PATTERN`
- [ ] `AS_PATTERN`
- [ ] `RANGE_PATTERN`
- [ ] `TYPED_PATTERN`
- [ ] `LAZY_PATTERN`
- [ ] `EXCEPTION_PATTERN`
- [ ] `PAREN_PATTERN`
- [ ] `POLY_VARIANT_PATTERN`
- [ ] `POLY_VARIANT_TYPE_PATTERN`
- [ ] `EFFECT_PATTERN`
- [ ] `LOCAL_OPEN_PATTERN`
- [ ] `OPERATOR_PATTERN`
- [ ] `FIRST_CLASS_MODULE_PATTERN`

### Type Expressions

- [ ] `TYPE_VAR`
- [ ] `TYPE_CONSTR`
- [ ] `TYPE_ALIAS`
- [ ] `TYPE_ARROW`
- [ ] `TYPE_TUPLE`
- [ ] `TYPE_PAREN`
- [ ] `TYPE_POLY_VARIANT`
- [ ] `POLY_VARIANT_TAG`
- [ ] `TYPE_PARAM`
- [ ] `TYPE_PARAMS`
- [ ] `TYPE_VARIANT_CONSTR`
- [ ] `TYPE_EXTENSIBLE`
- [ ] `TYPE_RECORD`
- [ ] `TYPE_RECORD_FIELD`
- [ ] `OBJECT_TYPE`
- [ ] `OBJECT_TYPE_FIELD`
- [ ] `LOCAL_OPEN_TYPE`
- [ ] `TYPE_CONSTRAINT`
- [ ] `POLY_TYPE`
- [ ] `MODULE_TYPE_EXPR`
- [ ] `FIRST_CLASS_MODULE_TYPE`
- [ ] `MODULE_TYPE_PATH`
- [ ] `FUNCTOR_PARAM`
- [ ] `FUNCTOR_TYPE`
- [ ] `MODULE_APPLICATION`
- [ ] `MODULE_UNIT_APPLICATION`

### Top-Level Declarations

- [x] `LET_BINDING`
- [ ] `LET_REC_BINDING`
- [ ] `LET_MUTUAL_DECL`
- [ ] `TYPE_DECL`
- [ ] `TYPE_MUTUAL_DECL`
- [ ] `EXCEPTION_DECL`
- [ ] `MODULE_DECL`
- [ ] `CLASS_DECL`
- [ ] `CLASS_TYPE_DECL`
- [ ] `MODULE_TYPE_DECL`
- [ ] `MODULE_TYPE_OF`
- [ ] `OPEN_STMT`
- [ ] `INCLUDE_STMT`
- [ ] `VAL_DECL`
- [ ] `EXTERNAL_DECL`

### Structural

- [x] `SOURCE_FILE`
- [x] `STRUCTURE`
- [ ] `SIGNATURE`
- [x] `MATCH_CASE`
- [x] `PATTERN_GUARD`
- [ ] `RECORD_FIELD`
- [ ] `RECORD_FIELD_PATTERN`
- [ ] `PARAMETER`
- [ ] `LOCALLY_ABSTRACT_TYPE_PARAM`
- [ ] `ARGUMENT`

### Error Recovery and Fallback

- [ ] `ERROR`
- [ ] `MISSING`
- [x] Refuse to format parser-recovery results when CST lifting fails

## Package Rules

### Std

- [ ] Constants like `3.14` should prefer `Std.Math.PI`
- [ ] Suggest `Result.protect`-style helpers instead of exceptions for flow control where appropriate
- [ ] Replace `x_of_y` / `string_of_int`-style names with the newer module APIs:
  - [ ] `string_of_int -> Int.to_string`
  - [ ] `int_of_string -> Int.parse`
  - [ ] `float_of_int -> Float.from_int`

## Tests

### Broken Workspace Test Inventory

When fixing these tests, since we have now access to `propane` for writing property tests, if any of these make more sense to be written as a property-test they should! 

Failing suites and cases:
- [ ] `std/std_data_base64_tests` (`8` failures): `encode simple`, `encode empty`, `encode bytes`, `decode simple`, `decode invalid char`, `roundtrip`, `binary roundtrip`, `padding`
- [ ] `std/std_diff_hashmap_tests` (`10` failures): `identical hashmaps`, `empty hashmaps`, `added keys`, `removed keys`, `changed values`, `mixed changes`, `nested hashmaps`, `one empty`, `different sizes`, `all different`
- [ ] `std/std_data_xml_tests` (`1` failure): `serialize with children`
- [ ] `std/std_data_toml_tests` (`4` failures): `parse bare string value`, `parse nested section names`, `detect missing equals`, ` parse duplicate keys in section (last wins)`
- [ ] `std/std_data_csv_tests` (`3` failures): `parse quoted field`, `parse quoted comma`, `parse escaped quote`
- [ ] `std/std_data_json_tests` (`4` failures): `parse integer`, `parse negative integer`, `parse float`, `parse scientific notatio n`
- [ ] `std/std_diff_vector_tests` (`10` failures): `identical vectors`, `empty vectors`, `added elements`, `removed elements`, `cha nged elements`, `different lengths`, `one empty`, `all different`, `nested vectors`, `mixed changes`
- [ ] `std/std_data_base85_tests` (`9` failures): `encode simple`, `encode empty`, `encode bytes`, `encode zeros`, `decode simple`, `decode zeros`, `decode invalid char`, `roundtrip`, `binary roundtrip`
- [ ] `std/std_diff_string_tests` (`10` failures): `identical strings`, `different strings`, `empty strings`, `one empty`, `char by char`, `inserted chars`, `deleted chars`, `replaced chars`, `case change`, `whitespace changes`
- [ ] `std/std_diff_list_tests` (`10` failures): `identical lists`, `empty lists`, `added elements`, `removed elements`, `changed e lements`, `reordered elements`, `nested lists`, `one empty`, `different lengths`, `mixed changes`
- [ ] `mime/rfc2231_tests` (`3` failures): `Simple filename parameter`, `RFC 2231 encoded filename with charset`, `RFC 2231 paramet er continuation`
- [ ] `blink/large_response_tests` (`3` failures): `large JSON response without truncation`, `streamed/chunked response without tru ncation`, `SSE event parsing`
- [ ] `syn/diagnostic_tests` (`26` failures): `0001_malformed_type_variable.ml`, `0005_type_missing_name.ml`, `0006_type_missing_eq uals.ml`, `0009_char_unclosed.ml`, `0010_char_empty.ml`, `0011_paren_unclosed.ml`, `0012_begin_unclosed.ml`, `0015_multiple_typ e_errors.ml`, `0016_mixed_let_type_errors.ml`, `0017_nested_paren_error.ml`, `0018_type_multiple_params_error.ml`, `0021_binop_ missing_right.ml`, `0024_bracketed_type_variables.ml`, `0025_list_unclosed.ml`, `0026_list_unclosed_empty.ml`, `0028_list_doubl e_semicolon.ml`, `0042_module_unclosed_struct.ml`, `0047_first_class_module_unclosed.ml`, `0048_module_type_constraint_missing_ eq.ml`, `0049_reference_assignment_missing_value.ml`, `0052_first_class_module_type_unclosed.ml`, `0053_module_type_constraint_ missing_type.ml`, `0055_module_type_unclosed_sig.ml`, `0059_consecutive_binary_operators.ml`, `0060_mutable_record_field_missin g_name.ml`, `0062_record_field_missing_colon.ml`
- [ ] `tusk-server/cache_tests` (`2` failures): `cache: fresh build, no cache`, `cache: second build, full package cache`
- [ ] `tusk-server/server_tests` (`2` failures): `cache: hit on rebuild`, `cache: invalidation on source change`
- [ ] `tusk-server/concurrent_tests` (`3` failures): `concurrent: different packages don't interfere`, `concurrent: same package bu ilds safely`, `concurrent: shared cache works correctly`
- [ ] `pubgrub/solver_tests` (`1` unique failing case, reported twice in the full run): `REF: Conflict with partial satisfier`
- [ ] `tusk-executor/caching_tests` (`1` failure): `cache store creation`
- [ ] `tusk-executor/package_builder_tests` (`fatal error`): `Stdlib.Effect.Unhandled(Miniriot__Proc_effect.Syscall("File.read", 1, _, -630647064))`
- [ ] `tusk-cli/build_lock_tests` (`1` failure): `build lock: waits for existing holder`
- [ ] `tty/comprehensive_tests` (`fatal error`): `Invalid_argument("index out of bounds")`
