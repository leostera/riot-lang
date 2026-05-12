# Raml JavaScript Backend Architecture

This document records the current subsystem shape of Melange's JS backend as
vendored in `3rdparty/melange/jscomp`.

The main lesson is that `jscomp` is not "a JS printer".
It is a full backend stack with:

- frontend bridge code
- global compiler hook mutation
- backend-owned per-file config and state
- a JS-colored middle IR
- a late JS IR
- a runtime ABI
- a cross-module artifact format
- import/export and package resolution logic

That matters for `raml`.

The useful thing to copy is the staged architecture.
The dangerous thing to copy is how early Melange lets JS semantics leak into
the backend pipeline.

## 1. Directory-Level Architecture

The backend is spread across a few directories with fairly clear roles.

### `common/`

This is the typed metadata layer for JS interop.

It mostly owns:

- FFI argument descriptors in `external_arg_spec`
- FFI call/module/object specs in `external_ffi_types`
- raw-JS payload types in `js_raw_info`
- backend constants and support types reused by `core/`

This directory is important because Melange does not treat JS externals as
ad-hoc strings all the way down.
It has real internal data for them, even though some of that data is still
serialized through source attributes.

### `core/`

This is the actual compiler backend.

It owns:

- the driver entrypoints in `js_implementation.cppo.ml`
- frontend hook installation in `initialization.cppo.ml`
- per-file AST/config ingestion
- the backend-local `Lam` IR
- the late JS `J` IR
- the `Lam` optimization and normalization passes
- `Lam -> J` lowering
- dependency discovery and refinement
- `.cmj` serialization
- JS module/import/export emission

This is the directory that matters most for `raml`.

### `runtime/`

This is the target runtime ABI for generated code.

It includes:

- `Caml_*` support modules
- `Curry`
- `Js_*` wrappers and typed public JS-facing modules
- runtime helpers for exceptions, options, modules, objects, OO support, and
  dynamic behavior the compiler relies on

### `stdlib/`

This is the JS-targeted stdlib surface compiled in Melange mode.

It is not just a library dependency.
It is part of the target contract that generated code expects to exist.

### Supporting Directories

There are also secondary directories:

- `melstd/`
  support library used by the compiler implementation itself
- `js_parser/`
  JS parsing support for parts of the FFI/raw-JS surface
- `others/`
  misc support code
- `test/`
  blackbox and unit coverage for the backend behavior

## 2. Whole Backend In One Picture

At a high level, the backend stack looks like this:

1. parse / load AST
2. rewrite AST and apply Melange config
3. typecheck
4. translate OCaml typed code to OCaml `Lambda`
5. convert `Lambda` to Melange `Lam`
6. normalize/optimize/group `Lam`
7. lower grouped `Lam` into structured JS output
8. build late JS IR `J.program`
9. shake/scope/flatten the JS IR
10. resolve final dependencies and package paths
11. build `.cmj` with delayed JS program
12. print CommonJS or ESM

There are several cross-cutting control planes around that pipeline:

- global frontend hook mutation
- per-file compiler state
- package/output configuration
- dependency and `.cmj` lookup
- runtime module naming

Those cross-cutting pieces are part of the architecture, not incidental glue.

## 3. Frontend Bridge And Control Plane

Melange does not sit behind a clean "typed IR in, JS out" boundary.

### `js_implementation.cppo.ml`

This is the main driver-facing bridge.

It still owns:

- interface compilation
- implementation compilation
- PPX application
- builtin AST rewrites
- typing
- `Lambda` extraction
- `.cmj` re-emission

So the JS backend is still tightly embedded in the OCaml compiler driver path.

### `initialization.cppo.ml`

This file is architectural, not cosmetic.

`Initialization.Global.run` mutates global OCaml compiler hooks such as:

- `Translcore.wrap_single_field_record`
- `Translmod.eval_rec_bindings`
- `Translmod.mangle_ident`
- `Typemod.should_hide`
- `Matching.*` helpers
- `Lambda` record/block helpers
- `Value_rec_compiler.compile_letrec`

`Initialization.Perfile` also owns:

- load path reset
- include-dir setup
- initial typing env creation

So Melange changes both:

- global compiler behavior
- per-compilation-unit environment state

### AST and Config Ingestion

Several files sit between parsed source and lowering:

- `pparse_driver`
- `cmd_ppx_apply`
- `builtin_ast_mapper`
- `ast_config`
- `ast_io`

Important consequence:

- source attributes such as `[@@@mel.config ...]` can directly mutate backend
  settings
- AST binary compatibility is part of backend ownership

## 4. Backend State And Metadata Subsystems

The backend has a real control plane, not just pure local transforms.

### Package / Output State

`js_packages_state` and `js_packages_info` own:

- package naming
- output module system
- output suffixes
- package-relative output paths
- file-case rules

This is how Melange decides whether the same compiled unit becomes:

- CommonJS
- ESM
- other configured output styles

### Dependency / Artifact State

`lam_compile_env` owns backend dependency bookkeeping such as:

- external JS module consolidation
- `.cmj` lookup for imported units
- purity queries
- package/case info loaded from `.cmj`
- final required-module population

`js_cmj_format` owns the backend artifact shape.

`artifact_extension` owns on-disk naming conventions.

`meldep` handles dependency output for driver-oriented flows.

### Runtime Naming

`js_runtime_modules.ml` is a hard-coded ABI vocabulary for runtime helper
modules such as:

- `Caml_option`
- `Caml_module`
- `Caml_exceptions`
- `Caml_obj`
- `Curry`

This is not a printer detail.
These names are chosen during lowering.

## 5. The IR Tower

Melange's backend is easier to reason about if you treat it as an IR tower.

### OCaml `Lambda`

This is still produced by the OCaml frontend.
Melange does not replace that step.

### Melange `Lam`

Defined in `core/lam.mli`.

This is the first backend-local IR.
It keeps many `Lambda`-like constructs:

- functions
- lets and letrecs
- switches
- loops
- mutation
- exceptions
- sends
- module globals

But it is already JS-colored through `lam_primitive`, which includes:

- `Pjs_call`
- `Pjs_object_create`
- `Pjs_apply`
- `Pimport`
- `Praw_js_code`
- JS option/null/undefined wrappers
- JS `typeof`
- JS function-length queries
- runtime-specific module-init helpers

So `Lam` is useful evidence for backend needs, but it is not a backend-neutral
shared IR.

### Grouped / Coerced `Lam`

`lam_coercion` and `lam_group` turn a big lambda into explicit top-level groups:

- `Single`
- `Recursive`
- `Nop`

This grouped layer is important because it decides:

- declaration form
- recursive initialization strategy
- export tracking shape
- `.cmj` summary boundaries

### `Js_output`

`js_output` is the structured result of `Lam` compilation before final
`J.program` assembly.

It carries:

- a JS block
- an optional value expression
- termination / "finished" state

This is a control-flow-aware codegen staging layer, not just a convenience
type.

### `J`

Defined in `core/j.ml`.

This is the late JS IR.
It is a JS subset specialized for this backend, not a general ESTree clone.

It owns:

- expressions and statements
- module references
- function bodies
- exports
- dependency-bearing module ids
- runtime-shaped forms such as `Caml_block`, `Optional_block`, and `Module`

### `J.deps_program`

This is the emission artifact right before printing.

It combines:

- the `J.program`
- required modules
- side-effect metadata
- preamble text

That is the thing both `.cmj` and final file emission care about.

## 6. The `Lam` Middle-End

`lam_compile_main.cppo.ml` is the orchestration center for the backend middle.

### Conversion

`Lam_convert.convert` lowers from OCaml `Lambda` into Melange `Lam`.

It does more than translation.
It also:

- decodes Melange FFI metadata
- introduces JS-specific primitives
- canonicalizes aliases
- tracks possible module dependencies
- threads dynamic-import state

This is one of the most important architecture facts in the whole backend:

the first backend-local IR conversion already performs JS semantic lowering.

### Normalization / Optimization Passes

The pipeline repeatedly uses modules such as:

- `lam_pass_deep_flatten`
- `lam_pass_collect`
- `lam_pass_exits`
- `lam_pass_remove_alias`
- `lam_pass_alpha_conversion`
- `lam_pass_lets_dce`

These are not optional cleanups.
The backend relies on them before `Lam -> J`.

### Grouping / Export Prep

`lam_coercion.coerce_and_group_big_lambda` produces:

- grouped top-level initialization
- export maps
- updated stats metadata

This stage is the real boundary between:

- normalized backend semantics
- JS declaration/program construction

## 7. `Lam -> J` Codegen Subsystems

The actual JS codegen path is distributed across several modules.

### Core Compilation

`lam_compile` owns the structured lowering of `Lam` into `Js_output`.

It handles:

- expression versus statement continuation shape
- tail position
- recursive lets
- control-flow lowering
- handler / jump table context

### Primitive Lowering

Several helpers own target-specific decisions:

- `lam_compile_primitive`
- `lam_compile_external_call`
- `lam_compile_external_obj`
- `lam_compile_dynamic_import`
- `js_of_lam_array`
- `js_of_lam_block`
- `js_of_lam_option`
- `js_of_lam_variant`
- `js_of_lam_module`

This is where many runtime representation choices become concrete JS shapes.

### JS IR Cleanup

Once a `J.program` exists, Melange runs passes such as:

- `js_pass_flatten`
- `js_pass_tailcall_inline`
- `js_pass_flatten_and_mark_dead`
- `js_pass_scope`
- `js_shake`

So the backend has both:

- a `Lam` middle-end
- a second JS-IR cleanup/shaking phase

## 8. Dependency Resolution And `.cmj`

Melange has a two-stage dependency story.

### Early Dependency Discovery

`Lam_convert` returns a conservative set of maybe-required modules.

That captures what lowering might need before later optimizations and shaking.

### Late Dependency Materialization

After `J.program` exists, Melange computes hard dependencies with:

- `Js_fold_basic.calculate_hard_dependencies`
- `Lam_compile_env.populate_required_modules`

That second step folds in:

- `.cmj` purity info
- runtime knowledge
- final usage after JS lowering and shaking

### `.cmj` As A Real Backend Artifact

`js_cmj_format` stores more than metadata.

It carries:

- exported values and arity
- optional persistent closed `Lam` for cross-module inlining
- purity
- package spec
- file-case info
- the delayed JS program itself

That is why `js_implementation.implementation_cmj` can re-emit JS from `.cmj`
without re-running the frontend.

This is one of the best Melange ideas to keep in spirit.

## 9. Import / Export And File Emission

Emission is its own subsystem.

### Path Resolution

`js_name_of_module_id` resolves module ids into import paths using:

- current package info
- dependency package info loaded from `.cmj`
- file-case info
- output module system
- output directory

This is not string decoration.
It is backend dependency materialization.

### Import / Export Printing

`js_dump_import_export` owns:

- CommonJS `require` generation
- ES module `import` generation
- export surface printing

### Program Dumping

`js_dump_program` prints `J.deps_program` with:

- imports
- preamble
- program body
- exports

`lam_compile_main.lambda_as_module` then writes one or more target files based
on the active output configuration.

## 10. Runtime And FFI Boundary

The runtime is part of code generation, not a post-hoc library add-on.

### Runtime ABI

Compiler lowering directly relies on runtime helpers for:

- option representation
- recursive module backpatching
- exceptions
- object/OO support
- currying
- JS exception wrapping

Representative modules include:

- `runtime/caml_option.ml`
- `runtime/caml_module.ml`
- `runtime/caml_exceptions.ml`
- `runtime/caml_obj.ml`
- `runtime/caml_oo.ml`

### Public JS Surface

`runtime/js.pre.ml` is mostly interface surface.

That is a good clue for `raml`:

- the user-facing JS API should mostly be typed declarations and externals
- the real runtime ABI should stay in explicit helper modules

### FFI Metadata Pipeline

FFI is structured across:

- `common/external_arg_spec`
- `common/external_ffi_types`
- `lam_ffi`
- `lam_compile_external_call`

Melange does have typed internal FFI metadata.
The main weakness is that the frontend path still serializes some of it through
JS-specific source attributes.

### Dynamic Import

Dynamic import is supported, but narrowly.

It is represented through:

- `Pimport`
- `dynamic_import` on module ids
- `lam_compile_dynamic_import`

It is module-loading specific, not a generic effect node.

## 11. What This Means For `raml`

Melange gives us the right warning labels.

### What To Borrow

- multiple explicit backend layers
- a real cross-module summary artifact
- explicit package/path resolution
- explicit runtime ABI modules
- a late JS-specific IR before final emission

### What Not To Borrow

- a JS-colored first shared IR
- global frontend hook mutation as the long-term backend API
- raw JS or JS option/null semantics in the shared middle
- stringly JS FFI payloads at the shared-IR boundary

## 12. Concrete Mapping To Current `raml`

The current `raml` split should stay cleaner than Melange's.

### Shared Layer

`compiler/raml-core/src/core_ir.ml` should stay backend-neutral.

That means:

- no raw JS
- no JS `undefined` / `null` meaning in shared semantics
- no `Pjs_*`-style primitive vocabulary
- no JS module path or import syntax choices

If `Core_ir` needs to grow for JS, the additions should describe semantic
facts, not JS encodings.

### JS-Specific Late Layer

`compiler/raml/src/jir.mli` is the right place for JS-only choices such as:

- import declarations
- runtime helper references
- JS module/export surface
- JS statement/expression distinctions needed by emission

`compiler/raml/src/js_emitter.ml` should remain a thin printer over that late
IR, not a place where runtime/import policy gets hardcoded.

### Hello World Guidance

The current `hello_world` problem should be read exactly this way:

- `println` availability is a typing/runtime-surface problem
- top-level side effects should use `Init_item.Eval`
- JS-specific runtime/import choices should land in `JIR`
- the emitter should only print the already-decided `JIR`

So the shortest sane first slice is:

1. make the `Std.println` surface available to typing
2. extend `JIR` with the minimum explicit import/runtime shape needed
3. lower `Init_item.Eval (Apply ...)` into a JS expression statement
4. emit one valid ESM story for `hello_world`
5. only then expand the surface area

That keeps the shared `Core_ir` honest while still learning the real lessons
from Melange's architecture.
