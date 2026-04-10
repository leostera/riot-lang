# Raml JavaScript Pipeline

This document records the current end-to-end shape of Melange's JavaScript
pipeline as seen from `3rdparty/melange/jscomp/core`.

It is the shortest map from "parsed OCaml source" to "emitted JS plus `.cmj`".

## 1. Driver Entry Points

The main entry point is `core/js_implementation.cppo.ml`.

There are three relevant paths.

### Interface path

- `Initialization.Perfile.init_path`
- parse the signature
- run PPX rewriters
- run builtin AST rewriting
- type the interface with `Typemod.type_interface`
- save `.cmi`

This path does not generate JS.

### Implementation path

- `Initialization.Perfile.init_path`
- parse the structure
- run PPX rewriters
- run builtin AST rewriting
- type the implementation with `Typemod.type_implementation`
- build OCaml Lambda with `Translmod.transl_implementation`
- simplify with `Lambda_simplif.simplify_lambda`
- call `Lam_compile_main.compile`
- write JS through `Lam_compile_main.lambda_as_module`

### `.cmj` re-emission path

- `Initialization.Perfile.init_path`
- load `.cmj` with `Js_cmj_format.from_file`
- take the stored `delayed_program`
- call `Lam_compile_main.lambda_as_module`

That last path is important.
The backend can regenerate JS from `.cmj` without re-running the frontend.

## 2. Stage Graph

The current JS pipeline stages are:

1. parse source or load binary AST
2. apply PPX and builtin AST rewrites
3. read per-file backend config such as `[@@@mel.config ...]`
4. typecheck
5. build OCaml Lambda
6. simplify OCaml Lambda
7. convert OCaml Lambda to Melange `Lam`
8. normalize and optimize `Lam`
9. coerce and group `Lam`
10. lower `Lam` to `J`
11. run JS IR cleanup passes
12. compute required modules and side-effect metadata
13. serialize `.cmj`
14. print CommonJS or ESM output

The important design fact is that the JS backend has several hard seams already.

The problem for `raml` is not lack of seams.
The problem is that some of the current seams are already JS-specific.

## 3. `package_info` Is Read Late On Purpose

`after_parsing_impl` reads `package_info` only after source processing.

The comment in `js_implementation` explains why:

- `[@@@mel.config { flags = [| ... |] }]` may change package specs

So package/output configuration is a semantic input to emission, not merely a
CLI detail glued on at the end.

## 4. The `Lam` Front Half

`Lam_compile_main.compile` starts by:

- getting export identifiers from `Translmod.get_export_identifiers`
- resetting `Lam_compile_env`
- calling `Lam_convert.convert`

`Lam_convert.convert` does more than a mechanical translation.

It also:

- canonicalizes aliases
- rewrites many OCaml primitives into backend-specific primitives
- decodes Melange FFI attributes
- tracks possible module dependencies
- carries `dynamic_import` state during recursive conversion

So the first backend-local IR step already performs semantic lowering, not just
syntactic copying.

## 5. The `Lam` Pass Order

The main `Lam` pipeline in `lam_compile_main` is approximately:

1. `Lam_convert.convert`
2. `Lam_pass_deep_flatten.deep_flatten`
3. `Lam_pass_collect.collect_info`
4. `Lam_pass_exits.simplify_exits`
5. `Lam_pass_remove_alias.simplify_alias`
6. `Lam_pass_deep_flatten.deep_flatten`
7. collect info again
8. simplify alias again
9. flatten again
10. collect info again
11. `Lam_pass_alpha_conversion.alpha_conversion`
12. `Lam_pass_exits.simplify_exits`
13. collect info again
14. simplify alias again
15. alpha conversion again
16. `Lam_pass_lets_dce.simplify_lets`
17. simplify exits again
18. `Lam_coercion.coerce_and_group_big_lambda`

Then the grouped program is compiled to JS.

The exact sequence is a little repetitive, but the important fact is clear:

- the backend relies on several normalization passes before `Lam -> J`
- alias cleanup, alpha conversion, exit simplification, and let DCE are part of
  the JS backend contract

## 6. Grouping Matters Before JS Lowering

`Lam_coercion.coerce_and_group_big_lambda` produces:

- groups of top-level bindings
- an export map

`Lam_group` then classifies each top-level piece as:

- `Single`
- `Recursive`
- `Nop`

This grouping is not cosmetic.

It shapes:

- JS declaration form
- recursive-binding handling
- export tracking
- `.cmj` summary generation

For `raml`, that suggests the shared pipeline probably wants an explicit
"grouped top-level initialization" stage before any backend printer.

## 7. `Lam -> J` Is Structured Compilation, Not Printing

`compile_group` delegates to `Lam_compile.compile_lambda` or
`Lam_compile.compile_recursive_lets`.

Those functions:

- track continuations and tail positions
- lower control flow to JS statements and expressions
- handle recursive functions and dummy updates
- lower primitives through `lam_compile_primitive`
- lower FFI through `lam_compile_external_call`
- special-case dynamic import through `lam_compile_dynamic_import`

The output of this stage is `Js_output.t`, which is then turned into `J.block`.

So the backend does not go straight from `Lam` to strings.
It still has one structured intermediate output layer before final `J.program`.

## 8. The JS Pass Order

After building a `J.program`, `lam_compile_main` runs these passes:

1. `Js_pass_flatten.program`
2. `Js_pass_tailcall_inline.tailcall_inline`
3. `Js_pass_flatten_and_mark_dead.program`
4. `Js_pass_scope.program`
5. `Js_shake.shake_program`

Each pass owns a different cleanup concern.

### `Js_pass_flatten`

Flattens nested statement structure into the simpler top-level shape expected by
later passes.

### `Js_pass_tailcall_inline`

Handles backend-specific tailcall rewriting/inlining on the JS IR.

### `Js_pass_flatten_and_mark_dead`

Performs more flattening, substitutes some immutable block field accesses, and
marks declarations as dead or used.

### `Js_pass_scope`

Computes function-local capture and scope information such as unused parameters
and unbounded variables.

### `Js_shake`

Performs tree-shaking over the JS IR by keeping only exported or effectful
definitions and their transitive free variables.

## 9. Dependency Collection Happens After JS Lowering

This is an important design detail.

`Lam_convert` only collects possible dependencies.
The final required module set is resolved after JS lowering using:

- `Js_fold_basic.calculate_hard_dependencies`
- `Lam_compile_env.populate_required_modules`

That second step uses module purity and other information from `.cmj` and
runtime knowledge to refine the final dependency set.

So the backend has both:

- an early conservative dependency collection
- a later, more precise dependency materialization

For `raml`, that suggests dependency discovery may need two layers too:

- shared semantic dependency discovery
- backend-specific import materialization after lowering decisions

## 10. `.cmj` Emission Is Interleaved With JS Emission Prep

Once the final dependency set and side-effect info are known, the backend builds
a `J.deps_program` containing:

- `program`
- `modules`
- `side_effect`
- `preamble`

Then it computes:

- module filename case via `Js_packages_info.module_case`
- `.cmj` export summary via `Lam_stats_export.export_to_cmj`

The `.cmj` stores the `delayed_program`, not just side metadata.

Only after that does `lambda_as_module` print JS.

## 11. Module System And File Emission

`lambda_as_module` writes the delayed JS program using the currently selected
`output_info`.

## 12. Current First Implemented `raml` JS Emission Slice

The current `raml` package now has its first explicit `JIR -> JS` printer.

This slice is intentionally fixed and small.

Today it emits one ESM-shaped source file with:

- ordered top-level `const`, `let`, and `var` declarations
- expression statements
- literal, identifier, and call expressions
- one trailing `export { ... }` block built from the `JIR` export table

That keeps the ownership boundary explicit:

- `JIR` still owns JS-shaped executable structure
- the emitter owns only source-printing for the supported `JIR` subset
- package config, import materialization, cleanup passes, and alternative module
  systems still do not exist in this slice

The first fixture family snapshots this pipeline as three separate outputs for
the same source fixture:

- `Raml Core IR`
- `JIR`
- final emitted JS

That includes:

- module system
  - `CommonJS`
  - `ESM`
  - `ESM_global`
- file suffix

`Js_dump_program` then:

- prints imports or requires
- prints the program body
- prints exports
- prints purity comments

Dynamic-import modules are excluded from the static `deps_program.modules`
import list and handled by explicit runtime lowering instead.

## 12. The Practical Stage Graph For `raml`

The current pipeline suggests a useful first decomposition for the JS path in
`raml`:

1. frontend source analysis and typed lowering
2. backend-neutral core IR
3. backend-neutral normalization and grouping
4. JS-specific lowering
5. JS-specific cleanup and dependency materialization
6. JS summary artifact
7. JS printing

The crucial change is not inventing more stages.

The crucial change is moving the backend-neutral/backend-specific boundary later
than Melange currently does.
