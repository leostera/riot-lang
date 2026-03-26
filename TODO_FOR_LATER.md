### syn Fixture Audit Plan

Goal: make `packages/syn/tests/fixtures` tight, high-signal, and cheap to maintain without weakening lossless or regression coverage.

Current audit snapshot (2026-03-26):
- `1017` source fixtures drive `2037` checked-in JSON expectations.
- Source fixtures are about `95.3 KB`; expectation snapshots are about `17.4 MB`.
- `745` fixtures are one-liners.
- Exact source duplicates have been fully removed (`0` exact duplicate source groups).
- `0` near-duplicate families remain at `0.95` similarity.
- Near-duplicate families at `0.88` similarity currently `0`.
- `types-and-signatures` is now down to `165` fixtures, `0` exact-duplicate fixtures.
- There are `0` reused numeric IDs.
- There were `0` cases where an exact-duplicate family existed only to preserve different lossless output.

Cleanup order:
- [x] Delete the `89` exact duplicate fixture triplets first.
- [x] Collapse near-duplicate micro-fixture families into one canonical fixture plus one deliberate edge-case fixture where needed.
- [x] Aggressively prune the generator-era `0500-0900` type matrix, especially the overlapping arrow, tuple, polymorphic, recursive, and record-definition families.
- [ ] Rebalance the suite toward repo invariants: lossless parsing, `.ml` vs `.mli`, module/signature structure, and bug-driven regressions.
- [x] Move fixture-generator scripts out of `packages/syn/tests/fixtures/`; they are tooling, not fixtures.
- [x] Add a fixture audit script for `syn`, modeled after `packages/krasny/tests/fixture_audit.py`, so duplicates and near-duplicates stay visible.

Target organization:
- [ ] Split fixtures by purpose instead of flat chronology.
- [ ] Learn from the krasny fixtures organization

Families that need the most attention:
- [x] Atom/operator microcases in `0000-0049`; many differ only by operator token or redundant parens.
- [x] List/tuple/record microcases in `0130-0240`; several are exact copies or near copies.
- [x] Type/declaration families in `0500-0900`; this is the biggest concentration of low-signal overlap.
- [x] Module/signature duplicates where identical sources were copied into later regression ranges.
- [x] Record-expression fixtures in `7000+`; several names imply span/newline differences but the source is byte-for-byte identical.

Done criteria:
- [x] No exact duplicate source fixtures remain.
- [x] No reused numeric fixture IDs remain if numbering is kept.
- [ ] Each remaining fixture has a clear role: smoke, lossless, interface, regression, upstream, or real-world.
- [ ] Lossless/trivia coverage is explicitly preserved rather than mixed into generic syntax buckets.
- [ ] `.mli` coverage is intentionally represented rather than incidental.
- [ ] A fixture audit script can report category counts, exact duplicates, and near-duplicate families in one command.

### ceibo Dedupe Follow-up

- [x] Upstream `syn`'s red-tree helper surface into `packages/ceibo`:
  `Red.new_token`, `SyntaxNode.children_list`, `SyntaxNode.direct_tokens`,
  `SyntaxNode.direct_nodes`, and `SyntaxNode.tokens`.
- [x] Delete `packages/syn/src/ceibo`; `packages/ceibo` is now the single
  source of truth for `syn`'s red-green tree surface.
- [ ] Fix the `tusk` incremental build bug for `syn` after the ceibo dedupe:
  `tusk build syn` can fail in a dirty incremental state after removing the
  vendored subtree, while `tusk clean && tusk build syn` succeeds.

## Verification Commands

1. Rebuild `tusk` when build-system, parser, or lint-runtime changes affect the binary:
   - `tusk build <pkg>`
3. After a package slice lands, rerun the focused test suites:
   - `tusk test <pkg>:cst_tests`
4. If the package has an expectation test runner, run it:
   - `timeout 900 python3 packages/<pkg>/tests/test_runner.py`

## krasny Checklist

Rough guidelines for formatting decisions:
* when writing tests prefer multiline strings (`{| ... |}`) which allow new linesover manually writing all the `\n` in them -- this makes it easier to visually scan the formatting change. So prefer tests like

    ```ocaml
    Test.case "breaks new lines between structure items" (fun () ->
        let source = {|
            let x = 1
            let y = 2
        |} in
        let expected =  {|
            let x = 1

            let y = 2
        |} in
        let actual = 
            parse_ml source 
            |> Krasny.format 
            |> Result.expect ~msg:"module structure should parse" 
        in
        Test.assert_equal ~expected ~actual;
        Ok ())

    ```
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
- current fixture corpus status: category corpus is `9/9` green, copied real-file regressions are `11/11` green, the unified manifest is `20/20` green overall, `krasny:format_tests` is `8/8`, and `syn:cst_tests` is `154/154`

### Trivia

- [x] `WHITESPACE`
- [x] `COMMENT`
- [x] `DOCSTRING`

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
- [x] `SEQUENCE_EXPR`
- [x] `PAREN_EXPR`
- [x] `TUPLE_EXPR`
- [x] `LIST_EXPR`
- [x] `ARRAY_EXPR`
- [ ] `RECORD_EXPR`
- [ ] `RECORD_UPDATE_EXPR`
- [ ] `UNREACHABLE_EXPR`
- [ ] `FIELD_ACCESS_EXPR`
- [x] `ARRAY_INDEX_EXPR`
- [x] `STRING_INDEX_EXPR`
- [ ] `ASSIGN_EXPR`
- [x] `CONSTRUCTOR_EXPR`
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

- [x] `TYPE_VAR`
- [x] `TYPE_CONSTR`
- [x] `TYPE_ALIAS`
- [x] `TYPE_ARROW`
- [x] `TYPE_TUPLE`
- [x] `TYPE_PAREN`
- [x] `TYPE_POLY_VARIANT`
- [x] `POLY_VARIANT_TAG`
- [x] `TYPE_PARAM`
- [x] `TYPE_PARAMS`
- [x] `TYPE_VARIANT_CONSTR`
- [x] `TYPE_EXTENSIBLE`
- [x] `TYPE_RECORD`
- [x] `TYPE_RECORD_FIELD`
- [x] `OBJECT_TYPE`
- [x] `OBJECT_TYPE_FIELD`
- [ ] `LOCAL_OPEN_TYPE`
- [x] `TYPE_CONSTRAINT`
- [x] `POLY_TYPE`
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
- [x] `LET_MUTUAL_DECL`
- [x] `TYPE_DECL`
- [x] `TYPE_MUTUAL_DECL`
- [ ] `EXCEPTION_DECL`
- [ ] `MODULE_DECL`
- [ ] `CLASS_DECL`
- [ ] `CLASS_TYPE_DECL`
- [ ] `MODULE_TYPE_DECL`
- [ ] `MODULE_TYPE_OF`
- [ ] `OPEN_STMT`
- [ ] `INCLUDE_STMT`
- [ ] `VAL_DECL`
- [x] `EXTERNAL_DECL`

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
- [x] `syn/diagnostic_tests` (`26` failures): `0001_malformed_type_variable.ml`, `0005_type_missing_name.ml`, `0006_type_missing_eq uals.ml`, `0009_char_unclosed.ml`, `0010_char_empty.ml`, `0011_paren_unclosed.ml`, `0012_begin_unclosed.ml`, `0015_multiple_typ e_errors.ml`, `0016_mixed_let_type_errors.ml`, `0017_nested_paren_error.ml`, `0018_type_multiple_params_error.ml`, `0021_binop_ missing_right.ml`, `0024_bracketed_type_variables.ml`, `0025_list_unclosed.ml`, `0026_list_unclosed_empty.ml`, `0028_list_doubl e_semicolon.ml`, `0042_module_unclosed_struct.ml`, `0047_first_class_module_unclosed.ml`, `0048_module_type_constraint_missing_ eq.ml`, `0049_reference_assignment_missing_value.ml`, `0052_first_class_module_type_unclosed.ml`, `0053_module_type_constraint_ missing_type.ml`, `0055_module_type_unclosed_sig.ml`, `0059_consecutive_binary_operators.ml`, `0060_mutable_record_field_missin g_name.ml`, `0062_record_field_missing_colon.ml`
- [ ] `tusk-server/cache_tests` (`2` failures): `cache: fresh build, no cache`, `cache: second build, full package cache`
- [ ] `tusk-server/server_tests` (`2` failures): `cache: hit on rebuild`, `cache: invalidation on source change`
- [ ] `tusk-server/concurrent_tests` (`3` failures): `concurrent: different packages don't interfere`, `concurrent: same package bu ilds safely`, `concurrent: shared cache works correctly`
- [ ] `pubgrub/solver_tests` (`1` unique failing case, reported twice in the full run): `REF: Conflict with partial satisfier`
- [ ] `tusk-executor/caching_tests` (`1` failure): `cache store creation`
- [ ] `tusk-executor/package_builder_tests` (`fatal error`): `Stdlib.Effect.Unhandled(Miniriot__Proc_effect.Syscall("File.read", 1, _, -630647064))`
- [ ] `tusk-cli/build_lock_tests` (`1` failure): `build lock: waits for existing holder`
- [ ] `tty/comprehensive_tests` (`fatal error`): `Invalid_argument("index out of bounds")`
