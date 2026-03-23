# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until its completed.
3. Mark a task complete in this document only after the relevant verification has passed.
4. DON'T FORGET TO GIT COMMIT AFTER EVERY SLICE! And use conevntional commit messages like: feat(pkg): <value delivered>

## TASKS

- [x] Go over the OCaml Structure Parity Checklist below and check all the boxes
- [x] Work on refactoring the existing lints
- [x] Refactor linting explanations to be in-depth with examples and not a structured "why | better | worse" garbage. Also explanations dont' need a title, its enough with the rule-id and the body 
- [ ] Work on implementing the remaining lints
- [ ] Work on fixing the broken tests

## Verification Commands

1. Rebuild `tusk` when build-system, parser, or lint-runtime changes affect the binary:
   - `tusk build tusk-cli`
2. Use the narrowest verification command that matches the change:
   - `tusk build syn`
   - `tusk build tusk-fix`
3. After a parser or CST slice lands, rerun the focused test suites:
   - `tusk test syn:cst_tests`
   - `tusk test tusk-fix:runner_tests`
4. After a CST syntax-family slice lands, optionally refreshing the fixture corpus if necessary:
   - `timeout 900 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
4. When making changes to the parser in general call:
   - `timeout 900 python3 packages/syn/tests/test_runner.py all`
5. When smoke-checking lint output, prefer the auto-rebuilding path:
   - `tusk run tusk -- fix --check --limit 10 <file>`

## tusk-fix Cleanup

- [x] Simplify built-in and package rule metadata to `id`, short description, and long explanation
- [x] Remove the runner_tests.ml code_id by refacotring the tests to pass in a `~rule` instead of `~code` and refactoring the assertions too
- [x] Keep explanations example-driven rather than structured as "why this rule exists"

## Next Built-in Lints

- [x] Prefer named closed polymorphic variants over inline closed polymorphic variants like ``[ `a | `b ] list``
- [x] Warn on bool positional parameters in functions; suggest a named parameter or an enum
- [x] Warn on tuples that should be records:
  - [x] more than 3 elements of the same type
  - [x] more than 4 elements of any type
- [x] Prefer scoped module qualification syntax over inline qualified field syntax:
  - [x] `let open Foo in [...]` -> prefer `Foo.[...]` unless there are multiple stacked local opens
  - [x] `Module.{ field = value }` over `{ Module.field = value }`
- [x] If a module has a single type definition, prefer it be called `t`
- [x] If a module has a public record type and accessor functions like
  - [x] `.mli`: `type t = { field : string }`
  - [x] `.mli`: `val field : t -> string`
  - [x] then suggest making the type opaque
- [x] Add a style rule encouraging record destructuring in function parameters for internal helpers like JSON serializers, so new fields are harder to ignore accidentally
  - covers both `let f record = let { a; b; _ } = record in ...` and repeated `record.field` helper bodies

## Package Rules

### Built-in Package Rules

- [x] Package names should be `kebab-case`
- [x] Package names should start with a letter
- [x] Package names should not have trailing dashes or underscores
- [x] Subdirectories and file names should be in `snake_case`
- [x] Warn about modules without `.mli` files

### Miniriot

- [x] Warn if a `while` or `for` loop does not immediately `yield ()`

### Std

- [x] `std:prefer-bang-equal-inequality`
- [x] `std:no-double-list-rev`
- [x] `ignore (List.map f xs)` / `ignore (Iter.map f iter)` should prefer the corresponding iterator form
- [x] `List.length x == 0` / `List.length x > 0` should prefer `List.is_empty`
- [ ] Constants like `3.14` should prefer `Std.Math.PI`
- [x] Prefer combinators over manual matches:
  - [x] `Option.map`
  - [x] `Result.map`
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
