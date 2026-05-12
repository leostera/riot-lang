# Raml Wasm To Zort Compatibility Notes

This document turns the Melange and `wasm_of_ocaml` findings into a concrete
view of what a `zort`-targeted wasm rewrite has to decide.

It is intentionally framed against:

- `zort/ARCHITECTURE.md`
- `zort/spec/compiler-runtime-integration.md`

because those files already describe the main rule `raml` should preserve:

- keep the semantic runtime core clean
- put compiler-compatibility and representation details in an outer layer

## 1. What Changes Relative To The Native Story

The native compatibility notes are dominated by raw machine ABI facts:

- tagged words
- stack frames
- polling ABI
- emitted native symbols

The wasm story changes that shape.

Upstream `wasm_of_ocaml` instead exposes:

- Wasm GC/reference-types value representation
- a JavaScript loader plus asset directory
- explicit runtime imports for boxed numeric families
- optional sidecar Wasm and JS primitive modules

So the main pressure shifts from:

- raw CPU ABI compatibility

to:

- Wasm representation compatibility
- host/loader compatibility
- primitive/import compatibility

## 2. Why `zort` Still Helps

`zort` already wants the runtime core to stay semantic:

- typed values
- heap refs
- heap store
- mutator
- collector
- control kernel
- capability-gated host substrate

That is a good fit for wasm too.

In fact, wasm has one advantage over the native compatibility problem:

- the backend does not have to start from raw machine words and assembly
  trampolines

That makes it more realistic to keep the `zort` core semantic while letting the
outer compatibility layer own the OCaml-ish or `raml`-specific encoding.

## 3. What `zort` Can Likely Own Directly

For a `raml` wasm target, `zort` should likely own:

- semantic heap object storage
- allocation and mutation policy
- collection
- explicit roots
- effect and continuation semantics
- primitive registry
- host capability gating

Those are exactly the things `zort` already wants to centralize.

## 4. What Must Still Live Outside The Semantic Core

Even on wasm, some things still belong in a compatibility/runtime layer above
the semantic kernel.

### Wasm value encoding

The layer needs to define how `raml` values map onto:

- immediates
- heap refs
- blocks
- bytes
- boxed numerics
- host/JS values

### Startup and module loading

The layer needs to own:

- module init ordering
- loader entrypoints
- asset manifests
- separate-compilation linking metadata

### Primitive imports

The layer needs to define:

- runtime helper imports
- user primitive imports
- host interop imports

### Host adaptation

The layer needs to make browser/Node/WASI differences explicit instead of
hiding them under one vague "wasm target".

### Effect strategy

If effects can be lowered through JSPI, CPS, or another strategy, that choice
belongs in the backend/runtime layer, not in the semantic kernel.

### First wasm-only layer after `Core_ir`

The wasm path should first lower directly from `Core_ir` into a wasm
runtime/host layer, not through a speculative shared post-`Core` IR.

That wasm-only layer should make explicit:

- Wasm GC/reference-types encoding for compiler value classes
- runtime-helper, user-primitive, and host-import signatures
- module startup materialization, loader entrypoints, and sidecar manifests
- one declared first host profile and one effect-lowering mode

Only after that layer should the pipeline diverge further into:

- final Wasm code generation
- binary shaping and packaging details
- alternative host profiles beyond the first declared one

## 5. First Host Choice Matters

The upstream `wasm_of_ocaml` story is clearly centered on JavaScript-hosted
Wasm:

- browser
- Node.js
- JS loader script

So if `raml` wants the shortest path to parity with that ecosystem, the first
wasm host should probably also be JS-hosted Wasm GC.

Pure `wasm32-wasi` can still matter later, especially because `zort` already
cares about capability-gated targets.

But it is not the shortest path to:

- JS interop
- upstream library expectations
- host-visible effects behavior

That first-host choice should be explicit.

## 6. A Sensible `raml` Staging Plan For Wasm

### Phase 1: freeze the shared `Raml Core IR` and wasm boundary

Decide explicitly:

- what the shared `Raml Core IR` preserves
- what the wasm-specific lowering input is
- what value classes the wasm backend must preserve
- what the first wasm host is

### Phase 2: no-allocation wasm smoke

Build one tiny path with:

- integers
- branches
- direct calls
- module entry

linked against a minimal wasm-facing `zort` runtime layer.

### Phase 3: allocation and basic heap objects

Add:

- tuples/blocks
- strings/bytes
- boxed numeric families
- initialization versus mutation rules

This proves the wasm value/runtime contract.

### Phase 4: primitive imports and JS interop

Add:

- runtime helper imports
- one small user primitive
- one small JS interop path

This proves the loader and host boundary.

### Phase 5: separate compilation artifacts

Add:

- per-module wasm summaries
- dependency metadata
- artifact manifests
- loader/link assembly for multiple units

This is the wasm equivalent of taking `.cmj` and Dune's separate compilation
seriously.

### Phase 6: exceptions and effects

Only after the runtime and import boundary is proven should `raml` bring over:

- exception propagation
- effect handlers
- continuation resume/reperform
- host-boundary edge cases

## 7. What Not To Do Early

### Do not pretend there is one generic "wasm target"

Browser, Node, and WASI have different capabilities and different library
stories.

### Do not force `zort` core to become an OCaml-shaped wasm runtime

Representation and import details belong in the compatibility layer.

### Do not reuse JS-specific IR as the wasm input

That only moves the problem around.

### Do not delay the artifact story

Separate compilation, sidecar files, and loader metadata are backend work from
the beginning.

## 8. The Most Important Conclusion

For `compiler/raml`, a wasm target on `zort` should be read as:

- a semantic `zort` core underneath
- one explicit wasm-facing compatibility/runtime layer above it
- one wasm-specific lowering pipeline fed by shared `Raml Core IR`

That is the cleanest way to target wasm without turning `zort` into a clone of
either Melange's JS runtime or OCaml's historical compatibility layers.
