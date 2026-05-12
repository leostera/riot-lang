# Native Compiler Runtime Integration

## Purpose

This note describes the compatibility surface `zort` needs in order to link
code emitted by the existing OCaml native compiler and observe it run.

This is a native-only contract.
It does not cover bytecode.

The goal is not "full OCaml runtime parity before first execution".
The goal is:

- identify the actual compiler/runtime seam,
- define the minimum compatibility layer needed for a first linked executable,
- keep that work outside the semantic kernel whenever possible.

## Design rule

- `zort` core remains semantic: typed values, `HeapRef`, `HeapStore`,
  `Mutator`, `Collector`, `ControlKernel`, `RuntimeServices`.
- existing-compiler support lives in a compatibility layer above that core.
- the compiler-facing layer may expose raw OCaml-shaped ABI details.
- the semantic kernel must not grow around those ABI details.

If supporting the existing compiler requires raw tagged words, frame tables,
native stack trampolines, or startup tables, that belongs in the compatibility
universe described by [ARCHITECTURE.md](../ARCHITECTURE.md), not in the core
runtime model.

## Source anchors

Compiler-side anchors:

- `vendor/ocaml/typing/primitive.mli`
- `vendor/ocaml/lambda/runtimedef.ml`
- `vendor/ocaml/lambda/translprim.ml`
- `vendor/ocaml/asmcomp/cmmgen.ml`
- `vendor/ocaml/asmcomp/cmm_helpers.ml`
- `vendor/ocaml/asmcomp/amd64/emit.mlp`
- `vendor/ocaml/asmcomp/arm64/emit.mlp`
- `vendor/ocaml/middle_end/compilenv.ml`
- `vendor/ocaml/asmcomp/asmlink.ml`

Runtime-side anchors:

- `vendor/ocaml/runtime/startup_nat.c`
- `vendor/ocaml/runtime/memory.c`
- `vendor/ocaml/runtime/alloc.c`
- `vendor/ocaml/runtime/frame_descriptors.c`
- `vendor/ocaml/runtime/dynlink_nat.c`
- `vendor/ocaml/runtime/amd64.S`
- `vendor/ocaml/runtime/arm64.S`
- `vendor/ocaml/runtime/caml/fiber.h`

Related `zort` notes:

- [primitive-boundary-and-native-dynlink.md](./primitive-boundary-and-native-dynlink.md)
- [gc-roots.md](./gc-roots.md)
- [gc-strategy.md](./gc-strategy.md)
- [effects-and-continuations.md](./effects-and-continuations.md)
- [exceptions-callbacks-and-backtraces.md](./exceptions-callbacks-and-backtraces.md)
- [platform-capabilities.md](./platform-capabilities.md)

## What the native compiler actually emits

The seam is concentrated in a few places.

### Primitive calls

- Primitive descriptions carry the native symbol name and native value
  representation policy.
- Lowering eventually emits `Cextcall(<native-name>, ...)`.
- Observable consequence: the runtime boundary is not just "lookup by OCaml
  primitive name". The compiler emits direct native symbol calls with an
  expected calling convention.

### Allocation and GC fast paths

- Native backends inline small allocation against `Domain_young_limit`.
- Slow paths branch into helpers such as:
  - `caml_call_gc`
  - `caml_alloc1`
  - `caml_alloc2`
  - `caml_alloc3`
  - `caml_allocN`
  - `caml_alloc_shr_check_gc`
- Observable consequence: a compiler-compatible runtime needs an allocation ABI
  and poll/GC ABI, not just a semantic `Runtime.allocTuple(...)` API.

### Mutation and remembered-set hooks

- Compiler-generated writes call:
  - `caml_modify`
  - `caml_initialize`
- Global mutable roots use:
  - `caml_modify_generational_global_root`
- Observable consequence: write barriers and initialization behavior are part of
  the compiler contract, not an internal implementation detail.

### Exceptions, callbacks, and effects

- Native code emits calls to:
  - `caml_raise_exn`
  - `caml_reraise_exn`
  - `caml_runstack`
  - `caml_perform`
  - `caml_reperform`
  - `caml_resume`
- Observable consequence: effect support for existing compiler output is partly
  an ABI and assembly-trampoline problem, not only a semantic control-kernel
  problem.

### Startup and metadata registration

- Linking emits and references:
  - `caml_program`
  - `caml_frametable`
  - per-unit `<unit>.gc_roots`
  - code-segment tables
- Native startup registers code fragments, frame tables, globals, and then runs
  the generated program entry through the runtime startup path.
- Observable consequence: "link and run" requires startup metadata ingestion,
  not just exported allocation helpers.

## First executable target

The first target should be intentionally narrow.

Pick one concrete native target triple first and keep it fixed until the
compiler/runtime bridge works end to end.
Do not try to land macOS, Linux, Windows, and WASI compiler-compatibility
simultaneously.

Recommended first target:

- the local development triple,
- native code only,
- one executable,
- no dynlink,
- no threads beyond the main domain,
- no external plugins,
- no callback into foreign code except explicitly chosen smoke primitives.

Current locked target:

- `aarch64-apple-darwin`
- existing compiler path:
  `~/.riot/toolchains/5.5.0-riot.3/aarch64-apple-darwin/bin/ocamlopt.opt`
- compatibility artifact: `libzort-caml-compat.dylib`
- current proven scope:
  - strict pure-startup objects link and run under `zort`
  - one strict compiler-emitted preallocated global-root fixture completes a
    no-allocation `caml_initialize` startup call under `zort`
  - one strict `-nostdlib -nopervasives` top-level external program links and
    runs under `zort`

### Milestone 0: linkable startup

Goal:

- produce a native executable that links against a `zort` compatibility shim,
- enters the startup path,
- reaches compiler-emitted code,
- returns without crashing.

This milestone does not need effects, dynlink, or broad primitive coverage.

Current status:

- achieved for a small cluster of narrow smokes on `aarch64-apple-darwin`
- the current successful fixtures are:
  - `e2e/ml/min_pure_startup.ml`
  - `e2e/ml/min_global_pair_root_zort.ml`
  - `e2e/ml/min_external_startup.ml`
- the current shim covers only:
  - startup entrypoints
  - `caml_program` handoff
  - `caml_c_call`
  - direct no-allocation `caml_initialize` stores into preallocated startup
    blocks
  - `caml_call_realloc_stack` stubbed for the non-growing case
  - a single top-level external symbol
- it does not yet cover stdlib startup, allocation, frame-descriptor ingestion,
  or general callback registration

### Milestone 1: no-allocation control smoke

Goal:

- run a tiny program that performs integer arithmetic and branching,
- returns success via exit code or a tiny host-observable primitive.

This proves:

- startup,
- native symbol linkage,
- exception path basics if used,
- compiler/runtime control handoff.

Current status:

- partially achieved through:
  - strict pure emitted startup without externals
  - a compiler-emitted no-allocation `caml_initialize` startup call into the
    compatibility layer
  - a top-level external primitive
- the next useful no-allocation step is a slightly richer pure emitted control
  case that still avoids heap allocation

### Milestone 2: allocation smoke

Goal:

- run a tiny allocating program using tuples and strings,
- survive collection,
- preserve roots correctly.

This proves:

- allocation slow paths,
- frame descriptors,
- root metadata,
- write barrier compatibility,
- compatibility value encoding.

### Milestone 3: primitive smoke

Goal:

- call one external primitive emitted by the existing compiler,
- route it through a typed `zort` primitive boundary,
- observe the result.

This proves:

- native-name symbol integration,
- callback-boundary mediation,
- argument/result codec correctness.

Effects, reperform, and continuation migration should be later milestones.
They are important, but they are not the shortest path to "compiler-emitted code
runs under zort".

## Minimum compatibility surface for Milestones 0-3

### 1. Startup shim

Need:

- compiler-facing startup entrypoints compatible with the chosen native path,
- loading of compiler-emitted metadata tables,
- deterministic fatal path for uncaught exceptions during startup,
- code-fragment registration policy explicit enough for backtraces and metadata.

Current `zort` status:

- semantic runtime startup exists in `src/runtime.zig`,
- compiler-facing native startup ABI does not.

### 2. Raw-value boundary codec

Need:

- OCaml-shaped raw value representation at the compiler boundary,
- encode/decode path between raw boundary values and semantic `Value`,
- clear handling for immediates, atoms, heap blocks, and stale handles.

Current `zort` status:

- partially available through `src/caml_compat/codec.zig`,
- current shim is handle-oriented and API-driven,
- compiler-emitted native code expects much lower-level ABI coupling than the
  current shim provides.

### 3. Allocation and poll ABI

Need:

- chosen-architecture support for the emitted allocation/poll contract,
- `Domain_young_limit` story or an intentional compiler-targeted substitute,
- `caml_call_gc` and small-allocation helper story,
- slow-path handoff into semantic `Mutator`/`Collector`.

Current `zort` status:

- semantic allocation and collection exist,
- compiler-facing fast-path ABI is missing.

### 4. Mutation / barrier ABI

Need:

- `caml_modify`,
- `caml_initialize`,
- generational global-root update hook where required,
- remembered-set correctness preserved through the boundary.

Current `zort` status:

- semantic mutation path exists in `src/mutator.zig`,
- compiler-facing raw-word mutation entrypoints are missing.

### 5. Metadata ingestion

Need:

- frame-table registration,
- `gc_roots` registration,
- startup-time linkage between unit metadata and runtime root providers,
- deterministic failure if required metadata is absent.

Current `zort` status:

- typed runtime root providers exist,
- compiler-emitted metadata ingestion is missing.

### 6. Primitive symbol bridge

Need:

- mapping from compiler-emitted native symbol names to typed `zort` primitive
  implementations,
- deterministic arity and lookup failures,
- callback-boundary mediation around primitive dispatch.

Current `zort` status:

- typed primitive registry exists in `src/primitive_registry.zig`,
- compatibility shim exists in `src/caml_compat/api.zig`,
- compiler-emitted direct native symbol bridge is still missing.

### 7. Exception and control trampoline policy

Need:

- exception raise path compatible with the chosen backend,
- minimal backtrace/frame-descriptor coherence,
- later, effect trampolines for `perform` / `reperform` / `resume`.

Current `zort` status:

- semantic exception/effect behavior exists,
- compiler-facing native trampolines are missing.

## What should explicitly wait

Do not block the first executable on:

- native dynlink parity,
- named-value parity across runtime instances,
- plugin unload,
- full effects parity,
- domains beyond the main domain,
- full signal-stack parity,
- broad stdlib/runtime-hosted primitive coverage.

Those are real features, but they are not the shortest path to proving that
`zort` can host compiler-emitted native code.

## Recommended implementation order

1. Lock one target triple and backend first.
2. Build a dedicated native-compiler compatibility shim module.
3. Land startup + metadata ingestion before broad primitive work.
4. Land allocation/poll ABI next.
5. Land write-barrier entrypoints next.
6. Route one external primitive through the typed registry.
7. Add e2e tests that compile a tiny OCaml native unit and link it against the
   shim.
8. Only then widen coverage toward exceptions, effects, and additional
   primitives.

## E2E acceptance criteria

We should treat compiler/runtime integration as an end-to-end feature, not a
bag of exported symbols.

The first e2e suite should cover:

- expected program output or exit status,
- expected trace shape for allocation / collection / primitive dispatch,
- focused benchmark signal for the smoke case,
- deterministic failure when required metadata or symbols are missing.

This should plug into the existing [e2e harness](../e2e/README.md), not create
a separate one-off test path.

## zort-specific compatibility stance

`zort` does not need to preserve every historical OCaml runtime quirk in order
to run existing compiler output.

It does need a compatibility layer that is honest about what the compiler
expects:

- raw value ABI,
- allocation/poll ABI,
- metadata tables,
- native symbol names,
- startup conventions,
- target-specific trampolines where the backend demands them.

That layer should be explicit, testable, and replaceable.
It should not become the new semantic center of the runtime.
