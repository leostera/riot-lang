# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until its completed.
3. Mark a task complete in this document only after the relevant verification has passed.
4. DON'T FORGET TO GIT COMMIT AFTER EVERY SLICE!

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

## Syn.Cst Fidelity

- [ ] Keep moving `Syn.Cst` toward a faithful `Ceibo -> Cst` lift driven by the fixture corpus
- [ ] Prefer adding real CST node shapes over convenience projections when a fixture fails
- [ ] Keep [packages/syn/src/cst.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/syn/src/cst.ml) focused on types, [packages/syn/src/cst_builder.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/syn/src/cst_builder.ml) focused on lifting, and [packages/syn/src/cst_json.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/syn/src/cst_json.ml) focused on snapshot serialization
- [~] Keep shrinking redundant record-accessor modules from `Syn.Cst`; family-level helpers may stay temporarily, but plain public records should be accessed directly
- [ ] Replace coarse `*_syntax_node` placeholders with typed CST where possible:
  - [ ] core types
  - [~] module types
  - [ ] class type bodies
  - [ ] signature-item internals
- [x] Replace flattened identifier/path segments with a recursive `Ident` CST
- [x] Make successful CST creation rule out public `Unknown` shapes by construction rather than by validation convention
- [ ] Add a stronger fixture/checklist pass for syntax that the stock parsetree distinguishes sharply
- [ ] Next parity slices:
  - [x] standalone attribute and extension items
  - [x] typed module-type tree
  - [x] typed module-expression tree
  - [~] attributed expressions and attributed items as faithful wrappers instead of token shells
    - [x] attribute and extension nodes now preserve sigil, qualified name, and payload anchors
    - [ ] expression/item/type wrappers still need typed payload structure instead of bare annotation nodes

### OCaml Structure Parity Checklist

When modelling the fields below, don't model them blindly, but consider meaningful groupings. For ex. instead of ExprString ExprInt ExprFloat we can group them by Expression.Literal(lit) where lit is its own literal enum with String | Int | Float | etc. Similarly, use good judgement to group similar things together.

Remember to document the CST types and constructors with examples so its easy to know what each one represents.

#### Completely Absent Today

- [ ] Core types:
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
- [ ] Patterns:
  - [x] type-constraint patterns
  - [x] effect patterns
  - [x] locally opened patterns
- [ ] Expressions:
  - [x] instance-variable assignment expressions
  - [ ] structure-item expressions
  - [ ] polymorphic expressions
  - [x] locally abstract type expressions
  - [ ] let-operator expressions
  - [ ] unreachable expressions
- [ ] Structure items:
  - [ ] type-extension items
  - [ ] recursive-module items
  - [x] standalone attribute items
  - [x] standalone extension items
- [ ] Signature items:
  - [ ] type declarations in interfaces
  - [ ] type substitutions in interfaces
  - [ ] type extensions in interfaces
  - [ ] exception declarations in interfaces
  - [ ] module declarations in interfaces
  - [ ] module substitutions in interfaces
  - [ ] recursive modules in interfaces
  - [ ] module type declarations in interfaces
  - [ ] module type substitutions in interfaces
  - [ ] open statements in interfaces
  - [ ] include statements in interfaces
  - [ ] class declarations in interfaces
  - [ ] class type declarations in interfaces
  - [ ] standalone attribute items in interfaces
  - [ ] standalone extension items in interfaces
- [ ] Module expressions:
  - [ ] module identifiers
  - [ ] structure module bodies
  - [ ] functor expressions
  - [ ] functor application expressions
  - [ ] unit functor application expressions
  - [ ] constrained module expressions
  - [ ] unpacked first-class modules
  - [ ] module extensions
- [ ] Module types:
  - [ ] module type identifiers
  - [ ] signature module types
  - [ ] functor module types
  - [ ] with-constraint module types
  - [ ] module-type-of expressions
  - [ ] module type extensions
  - [ ] module type aliases
- [ ] Class expressions:
  - [ ] class constructor references
  - [ ] class structure bodies
  - [ ] function-style class expressions
  - [ ] class application expressions
  - [ ] let-bound class expressions
  - [ ] constrained class expressions
  - [ ] class extensions
  - [ ] locally opened class expressions
- [ ] Class types:
  - [ ] class type constructor references
  - [ ] class signatures
  - [ ] arrow-style class types
  - [ ] class type extensions
  - [ ] locally opened class types
- [ ] Class fields:
  - [ ] class constraints
  - [ ] class-field attributes
  - [ ] class-field extensions
- [ ] Class type fields:
  - [ ] inherited class type fields
  - [ ] value declarations in class types
  - [ ] method declarations in class types
  - [ ] class type constraints
  - [ ] class-type-field attributes
  - [ ] class-type-field extensions

#### Present But Opaque

- [ ] Core types still have opaque branches and missing families even though the first typed `core_type` tree is in place
- [x] First-class module expressions, `let module`, and module declarations now lift through `module_expression`
- [ ] Module types are still raw syntax in places like:
  - [x] first-class module expressions / patterns / type definitions now lift through `module_type`
  - [ ] class type bodies and some declaration sites still keep raw syntax
- [ ] Type definitions are still opaque in several branches:
  - [ ] `TypeDefinition.Alias`
  - [ ] `TypeDefinition.Object`
  - [ ] `TypeDefinition.FirstClassModule`
  - [ ] `TypeDefinition.Other`
- [ ] Type declarations still do not expose typed structure for:
  - [ ] manifest/core type of aliases
  - [ ] constraints
  - [ ] variance/injectivity on parameters
  - [ ] record label declarations
  - [ ] constructor argument lists
  - [ ] constructor result types / GADT constructors
- [ ] Attributes and extensions are still token shells rather than structured payloads
- [~] Attributes and extensions now preserve sigils, names, and payload anchors, but their payloads are not yet fully typed
- [~] Includes, opens, and with-constraints are still thinner than they should be
  - [x] include statements now carry typed module-expression/module-type targets
  - [ ] open statements still only preserve qualified identifiers
  - [ ] with-constraints still do not form a richer typed tree

#### Present But Lossy

- [ ] Patterns:
  - [ ] tuple patterns are missing labelled elements and open tuple `...` structure
  - [ ] record patterns are missing open-vs-closed structure
  - [ ] constructor patterns are missing existential type variables
  - [ ] polyvariant patterns are simplified to tag + optional payload
  - [ ] literal patterns lose delimiter/suffix detail and exact constant structure
  - [ ] interval patterns store two tokens instead of parsed constants
  - [ ] first-class-module patterns are still thinner than the stock unpack form
  - [ ] Pattern attributes are represented as wrapper nodes instead of orthogonal metadata
- [ ] Expressions:
  - [ ] constructor expressions are likely flattened into `Path` or `Apply`
  - [ ] field assignment expressions are only approximated via `Assign (FieldAccess ...)`
  - [ ] object override expressions are only approximated via `ObjectUpdate`
  - [ ] function expressions only store cases, not the richer parameter/type structure
  - [ ] `for` loops store a direction token instead of a typed direction flag
  - [ ] record expressions only keep syntax-level field paths and optional values
  - [ ] packed first-class module expressions are still mostly raw syntax
  - [ ] Expression attributes are represented as wrapper nodes instead of orthogonal metadata
- [ ] Type declarations:
  - [ ] `private_flag` is missing
  - [ ] parameter variance/injectivity is missing
  - [ ] `type_kind` detail is still compressed
  - [ ] record fields only store names + mutability, not field types and attrs
  - [ ] variant constructors only store names, not arguments/results/attrs
  - [x] polyvariant tags only store names, not payload types or closedness
- [ ] Structure items:
  - [ ] implementation and interface currently share the same `Item.t`, so signature structure is lossy
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
