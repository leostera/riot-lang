# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until its completed.
3. Mark a task complete in this document only after the relevant verification has passed.
4. DON'T FORGET TO GIT COMMIT AFTER EVERY SLICE! And use conevntional commit messages like: feat(pkg): <value delivered>

## TASKS

- [ ] Go over the OCaml Structure Parity Checklist below and check all the boxes
- [ ] Strengthen the CST corpus so we can compare `Syn.Cst` structure against the stock OCaml parsetree more confidently
- [ ] Work on refactoring the existing lints
- [ ] Work on implementing the remaining lints

## Verification Commands

1. Rebuild `tusk` when build-system, parser, or lint-runtime changes affect the binary:
   - `timeout 240 tusk build tusk-cli`
2. Use the narrowest verification command that matches the change:
   - `timeout 60 tusk build syn`
   - `timeout 60 tusk build tusk-fix`
3. After a parser or CST slice lands, rerun the focused test suites:
   - `timeout 180 tusk test syn:cst_tests`
   - `timeout 180 tusk test tusk-fix:runner_tests`
4. After a CST syntax-family slice lands, optionally refreshing the fixture corpus if necessary:
   - `timeout 900 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
4. When making changes to the parser in general call:
   - `timeout 900 python3 packages/syn/tests/test_runner.py all`
5. When smoke-checking lint output, prefer the auto-rebuilding path:
   - `timeout 180 tusk run tusk -- fix --check --limit 10 <file>`
- `tusk build tusk-cli`
- `tusk build syn`
- `tusk build tusk-fix`
- `tusk test syn:cst_tests`
- `tusk test tusk-fix:runner_tests`
- `timeout 900 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
- `./tusk fix --list-rules`
- `./tusk fix --list-diagnostics`
- `tusk run tusk -- fix --check --limit 10 <file>`

## Things to Keep in Mind

- Keep moving `Syn.Cst` toward a faithful `Ceibo -> Cst` lift driven by the fixture corpus
- Prefer adding real CST node shapes over convenience projections when a fixture fails
- Keep [packages/syn/src/cst.ml] focused on types, [packages/syn/src/cst_builder.ml] focused on lifting, and [packages/syn/src/cst_json.ml] focused on snapshot serialization
- Keep shrinking redundant record-accessor modules from `Syn.Cst`; family-level helpers may stay temporarily, but plain public records should be accessed directly

## OCaml Structure Parity Checklist

When modelling the fields below, don't model them blindly, but consider meaningful groupings. For ex. instead of ExprString ExprInt ExprFloat we can group them by Expression.Literal(lit) where lit is its own literal enum with String | Int | Float | etc. Similarly, use good judgement to group similar things together.

Remember to document the CST types and constructors with examples so its easy to know what each one represents.

- [x] Split Item.t into StructureItem.t and SignatureItem.t to make those enums/variants less likely to be mixed up
- [x] Remove the convenience views from  type implementation and type interface, since the ordering in which these let bindings and expressions occur in the source tree is relevant so they can't be interweaved. If someone wants to find the let bindings or val signatures, etc, they have to iterate over the items (SignatureITem.t or StructureItem.t)

- [x] Expression.Fun and Expression.Function are fundamentally different: the `function | ... -> ...` expresion doesn't have parameters! it only has _cases_ and each case basically has a single parameter, like `function | x -> x` or `function | 1 -> 2` -- only the `fun x -> x` syntax and the `let f x = x` syntax have parameters. They can be in the same Expression.Function constructor, but internally they probably should be 2 different records/variants since they don't really overlap, wdyt?

#### Completely Absent Today

- [x] Core types:
  - [x] wildcard types
  - [x] type variable references
  - [x] arrow types
  - [x] tuple types
  - [x] constructor-applied types
  - [x] object types
  - [x] class types in type positions
  - [x] aliased types
  - [x] polyvariant types
  - [x] universally quantified types
  - [x] package types
  - [x] locally opened types
  - [x] extension types
  - [x] package-type payloads
  - [x] row fields
  - [x] object fields
- [x] Patterns:
  - [x] type-constraint patterns
  - [x] effect patterns
  - [x] locally opened patterns
- [x] Expressions:
  - [x] instance-variable assignment expressions
  - [x] structure-item expressions
  - [x] polymorphic expressions
  - [x] locally abstract type expressions
  - [x] let-operator expressions
  - [x] unreachable expressions
- [x] Structure items:
  - [x] type-extension items
  - [x] recursive-module items
  - [x] standalone attribute items
  - [x] standalone extension items
- [x] Signature items:
  - [x] type declarations in interfaces
  - [x] type substitutions in interfaces
  - [x] type extensions in interfaces
  - [x] exception declarations in interfaces
  - [x] module declarations in interfaces
  - [x] module substitutions in interfaces
  - [x] recursive modules in interfaces
  - [x] module type declarations in interfaces
  - [x] module type substitutions in interfaces
  - [x] open statements in interfaces
  - [x] include statements in interfaces
  - [x] class declarations in interfaces
  - [x] class type declarations in interfaces
  - [x] standalone attribute items in interfaces
  - [x] standalone extension items in interfaces
- [x] Module expressions:
  - [x] module identifiers
  - [x] structure module bodies
  - [x] functor expressions
  - [x] functor application expressions
  - [x] unit functor application expressions
  - [x] constrained module expressions
  - [x] unpacked first-class modules
  - [x] module extensions
- [x] Module types:
  - [x] module type identifiers
  - [x] signature module types
  - [x] functor module types
  - [x] with-constraint module types
  - [x] module-type-of expressions
  - [x] module type extensions
  - [x] module type aliases
- [x] Class expressions:
  - [x] class constructor references
  - [x] class structure bodies
  - [x] function-style class expressions
  - [x] class application expressions
  - [x] let-bound class expressions
  - [x] constrained class expressions
  - [x] class extensions
  - [x] locally opened class expressions
- [x] Class types:
  - [x] class type constructor references
  - [x] class signatures
  - [x] arrow-style class types
  - [x] class type extensions
  - [x] locally opened class types
- [x] Class fields:
  - [x] class constraints
  - [x] class-field attributes
  - [x] class-field extensions
- [x] Class type fields:
  - [x] inherited class type fields
  - [x] value declarations in class types
  - [x] method declarations in class types
  - [x] class type constraints
  - [x] class-type-field attributes
  - [x] class-type-field extensions

#### Present But Opaque

- [x] Core types still have opaque branches and missing families even though the first typed `core_type` tree is in place
- [x] First-class module expressions, `let module`, and module declarations now lift through `module_expression`
- [x] Module types are still raw syntax in places like:
  - [x] first-class module expressions / patterns / type definitions now lift through `module_type`
  - [x] class type bodies and some declaration sites still keep raw syntax
- [x] Type definitions are still opaque in several branches:
  - [x] `TypeDefinition.Alias`
  - [x] `TypeDefinition.Object`
  - [x] `TypeDefinition.FirstClassModule`
  - [x] public `TypeDefinition.Other` has been removed
- [x] Type declarations still do not expose typed structure for:
  - [x] manifest/core type of aliases
  - [x] constraints
  - [x] variance/injectivity on parameters
  - [x] record label declarations
  - [x] constructor argument lists
  - [x] constructor result types / GADT constructors
- [x] Attributes and extensions are still token shells rather than structured payloads
- [~] Attributes and extensions now preserve sigils, names, and payload anchors, but their payloads are not yet fully typed
- [x] Includes, opens, and with-constraints are still thinner than they should be
  - [x] include statements now carry typed module-expression/module-type targets
  - [x] open statements now distinguish signature-style paths from implementation module-expression targets
  - [x] with-constraints still do not form a richer typed tree

#### Present But Lossy

- [x] Patterns:
  - [x] tuple patterns are missing labelled elements and open tuple `...` structure
  - [x] record patterns are missing open-vs-closed structure
  - [x] constructor patterns are missing existential type variables
  - [x] polyvariant patterns are simplified to tag + optional payload
  - [x] literal patterns lose delimiter/suffix detail and exact constant structure
  - [x] interval patterns store two tokens instead of parsed constants
  - [x] first-class-module patterns are still thinner than the stock unpack form
  - [x] Pattern attributes are represented as wrapper nodes instead of orthogonal metadata
- [ ] Expressions:
  - [x] constructor expressions are likely flattened into `Path` or `Apply`
  - [x] field assignment expressions are only approximated via `Assign (FieldAccess ...)`
  - [x] object override expressions are only approximated via `ObjectUpdate`
  - [x] function expressions only store cases, not the richer parameter/type structure
  - [x] `for` loops store a direction token instead of a typed direction flag
  - [x] record expressions only keep syntax-level field paths and optional values
  - [x] packed first-class module expressions are still mostly raw syntax
  - [ ] Expression attributes are represented as wrapper nodes instead of orthogonal metadata
- [ ] Type declarations:
  - [x] `private_flag` is missing
  - [x] parameter variance/injectivity is missing
  - [ ] `type_kind` detail is still compressed
  - [x] record fields only store names + mutability, not field types and attrs
  - [~] variant constructors now expose result types, but constructor attributes are still missing
  - [x] polyvariant tags only store names, not payload types or closedness
- [ ] Structure items:
  - [x] implementation and interface currently share the same `Item.t`, so signature structure is lossy
  - [ ] item-level attributes and extension payloads are not modeled like Parsetree
- [ ] Metadata:
  - [ ] locations are not modeled orthogonally like `Location.t` / `location_stack`
  - [ ] many typed flags are represented as tokens/bools or dropped entirely
  - [ ] attributes are not attached orthogonally across the tree the way Parsetree does

## tusk-fix Cleanup

- [ ] Simplify rule metadata so a rule really only needs:
  - [ ] `id`
  - [ ] short description
  - [ ] long explanation
- [ ] Keep explanations example-driven rather than structured as "why this rule exists"
- [ ] Group built-in lints by category, similar to Clippy
- [ ] Do a learning pass on how Clippy organizes and authors lints

## Next Built-in Lints

- [ ] Prefer named closed polymorphic variants over inline closed polymorphic variants like ``[ `a | `b ] list``
- [ ] Warn on bool positional parameters in functions; suggest a named parameter or an enum
- [ ] Warn on tuples that should be records:
  - [ ] more than 3 elements of the same type
  - [ ] more than 4 elements of any type
- [ ] Prefer scoped module qualification syntax over inline qualified field syntax:
  - [ ] `let open Foo in [...]` -> prefer `Foo.[...]` unless there are multiple stacked local opens
  - [ ] `Module.{ field = value }` over `{ Module.field = value }`
- [ ] If a module has a single type definition, prefer it be called `t`
- [ ] If a module has a public record type and accessor functions like
  - [ ] `.mli`: `type t = { field : string }`
  - [ ] `.mli`: `val field : t -> string`
  - [ ] then suggest making the type opaque
- [ ] Add a style rule encouraging record destructuring in function parameters for internal helpers like JSON serializers, so new fields are harder to ignore accidentally

## Package Rules

### Built-in Package Rules

- [ ] Package names should be `kebab-case`
- [ ] Package names should start with a letter
- [ ] Package names should not have trailing dashes or underscores
- [ ] Subdirectories and file names should be in `snake_case`
- [ ] Warn about modules without `.mli` files

### Miniriot

- [ ] Warn if a `while` or `for` loop does not immediately `yield ()`

### Std

- [x] `std:prefer-bang-equal-inequality`
- [x] `std:no-double-list-rev`
- [ ] `ignore (List.map f xs)` / `ignore (Iter.map f iter)` should prefer the corresponding iterator form
- [ ] `List.length x == 0` / `List.length x > 0` should prefer `List.is_empty`
- [ ] Constants like `3.14` should prefer `Std.Math.PI`
- [ ] Prefer combinators over manual matches:
  - [ ] `Option.map`
  - [ ] `Result.map`
- [ ] Suggest `Result.protect`-style helpers instead of exceptions for flow control where appropriate
- [ ] Replace `x_of_y` / `string_of_int`-style names with the newer module APIs:
  - [ ] `string_of_int -> Int.to_string`
  - [ ] `int_of_string -> Int.parse`
  - [ ] `float_of_int -> Float.from_int`

## Tests

### Broken Workspace Test Inventory

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
