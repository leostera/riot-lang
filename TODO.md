# TODO

## Working Loop

1. Rebuild `tusk` when build-system, parser, or lint-runtime changes affect the binary:
   - `rm -f _build/tusk.lock`
   - `timeout 240 tusk build tusk-cli`
2. Use the narrowest verification command that matches the change:
   - `rm -f _build/tusk.lock`
   - `timeout 60 tusk build syn`
   - `rm -f _build/tusk.lock`
   - `timeout 60 tusk build tusk-fix`
3. After a parser or CST slice lands, rerun the focused test suites:
   - `rm -f _build/tusk.lock`
   - `timeout 180 tusk test syn:cst_tests`
   - `rm -f _build/tusk.lock`
   - `timeout 180 tusk test tusk-fix:runner_tests`
4. After a CST syntax-family slice lands, refresh the fixture corpus:
   - `timeout 900 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
5. When smoke-checking lint output, prefer the auto-rebuilding path:
   - `rm -f _build/tusk.lock`
   - `timeout 180 tusk run tusk -- fix --check --limit 10 <file>`
6. Read this file from top to bottom and pick the next unchecked item that is unblocked.
7. Mark a task complete only after the relevant verification has passed.
8. Commit often, with one logical slice per commit.

## Verification Commands

- `rm -f _build/tusk.lock && timeout 240 tusk build tusk-cli`
- `rm -f _build/tusk.lock && timeout 60 tusk build syn`
- `rm -f _build/tusk.lock && timeout 60 tusk build tusk-fix`
- `rm -f _build/tusk.lock && timeout 180 tusk test syn:cst_tests`
- `rm -f _build/tusk.lock && timeout 180 tusk test tusk-fix:runner_tests`
- `timeout 900 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
- `rm -f _build/tusk.lock && timeout 120 ./tusk fix --list-rules`
- `rm -f _build/tusk.lock && timeout 120 ./tusk fix --list-diagnostics`
- `rm -f _build/tusk.lock && timeout 180 tusk run tusk -- fix --check --limit 10 <file>`

## Current State

- [x] The `Syn.Cst` fixture corpus is green: `1180 passed, 0 failed`
- [x] The current CST fixture frontier has been driven to `0`
- [ ] Strengthen the CST corpus so we can compare `Syn.Cst` structure against the stock OCaml parsetree more confidently

## Syn.Cst Fidelity

- [ ] Keep moving `Syn.Cst` toward a faithful `Ceibo -> Cst` lift driven by the fixture corpus
- [ ] Prefer adding real CST node shapes over convenience projections when a fixture fails
- [ ] Keep [packages/syn/src/cst.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/syn/src/cst.ml) focused on types, [packages/syn/src/cst_builder.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/syn/src/cst_builder.ml) focused on lifting, and [packages/syn/src/cst_json.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/syn/src/cst_json.ml) focused on snapshot serialization
- [ ] Replace coarse `*_syntax_node` placeholders with typed CST where possible:
  - [ ] core types
  - [ ] module types
  - [ ] class type bodies
  - [ ] signature-item internals
- [ ] Replace flattened `ModulePath.segments` with a recursive path CST
- [ ] Make successful CST creation rule out public `Unknown` shapes by construction rather than by validation convention
- [ ] Add a stronger fixture/checklist pass for syntax that the stock parsetree distinguishes sharply

### Completed CST Syntax Families

- [x] destructuring `let` bindings and mutual `let`
- [x] record/update/index/assignment expressions
- [x] type annotations and coercions
- [x] loops / begin / assert / lazy expressions
- [x] lazy / exception / range patterns
- [x] object syntax / class declarations / method calls
- [x] module/signature / first-class module coverage
- [x] operator patterns / binding operators
- [x] attributes / extensions / docstrings
- [x] rawidents / multi-indices / remaining parser edge cases
- [x] include / val / external / signature items

## tusk-fix Cleanup

- [ ] Simplify rule metadata so a rule really only needs:
  - [ ] `id`
  - [ ] short description
  - [ ] long explanation
- [ ] Keep explanations example-driven rather than structured as "why this rule exists"
- [ ] Group built-in lints by category, similar to Clippy
- [ ] Do a learning pass on how Clippy organizes and authors lints

## Implemented Built-in Lints

- [x] `snake-case-type-names`
- [x] `descriptive-type-variables`
- [x] `snake-case-function-names`
- [x] `class-case-module-names`
- [x] `snake-case-variable-names`
- [x] `no-prime-variables`
- [x] `snake-case-argument-names`
- [x] `prefer-multiline-string-literals`
- [x] `no-custom-operators`
- [x] `ordered-argument-kinds`
- [x] `t-first-named-arguments`
- [x] `alphabetized-named-arguments`
- [x] `snake-case-record-fields`
- [x] `class-case-constructors`
- [x] `snake-case-polyvariant-tags`
- [x] `avoid-single-letter-function-names`
- [x] `avoid-single-letter-type-names`
- [x] `no-inline-parameter-type-annotations`
- [x] `no-function-shorthand`
- [x] `limit-parenthesis-depth`
- [x] `limit-function-parameters`
- [x] `prefer-pipelines-for-nested-calls`
- [x] `no-redundant-else-unit`
- [x] `no-open-bang`
- [x] `prefer-if-over-bool-match`
- [x] `no-redundant-reraise`
- [x] `no-useless-let-return`
- [x] `no-unnecessary-rec`
- [x] `limit-open-statements`
- [x] `prefer-sequences-over-let-unit`
- [x] `no-eta-expansion`
- [x] `no-redundant-parentheses`
- [x] `no-redundant-begin-end`
- [x] `no-public-mutable-fields`
- [x] `prefer-scoped-field-access`
- [x] `limit-nested-match-depth`
- [x] `no-exn-suffix-functions`
- [x] `no-boolean-comparisons-in-conditionals`

## Remaining Built-in Lints

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
