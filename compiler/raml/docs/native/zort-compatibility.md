# Raml To Zort Compatibility Notes

This document turns the `lambda` and `asmcomp` analysis into a concrete view of
what a `zort`-targeted rewrite must decide.

It is intentionally framed against:

- `zort/spec/compiler-runtime-integration.md`
- `zort/BACKLOG.md`

because those files already describe the desired high-level direction:

- keep compiler compatibility in a dedicated compatibility layer
- avoid distorting the semantic `zort` core
- stage from startup to allocation, then primitives, then exceptions and
  effects

## 1. What The Existing Native Backend Assumes

From `vendor/ocaml/lambda` and `vendor/ocaml/asmcomp`, the existing native
backend assumes at least the following runtime contracts.

### Raw value representation

The backend assumes:

- tagged immediates
- block headers with tags and sizes
- infix pointers
- closure-info words
- boxed float and boxed integer layouts

This assumption is visible in:

- Lambda primitives
- `cmm_helpers`
- closure layout code in `cmmgen`

### Allocation and polling ABI

The backend assumes:

- allocation fast paths or allocation helpers
- polling operations
- GC-safe rules around derived pointers and roots
- allocation without arbitrary GC between specific metadata writes and field
  initialization

This shows up in:

- `Ppoll`
- `Calloc`
- `Mach.Ialloc`
- polling instrumentation
- stack-frame analysis

### Mutation and barrier entrypoints

The backend assumes a distinction between:

- initialization
- heap initialization
- root initialization
- assignment

and eventually expects runtime-visible behavior for field updates and roots.

### Control-flow trampolines

Lambda already knows about:

- `Prunstack`
- `Pperform`
- `Presume`
- `Preperform`

So effects and continuation control are not purely "future runtime details".
They are part of the backend surface.

### Metadata tables and symbol conventions

The backend emits or expects:

- unit entry symbols
- preallocated blocks
- GC roots tables
- generic apply/send/curry helpers
- linker-visible primitive symbols

## 2. What `zort` Already Wants

`zort/spec/compiler-runtime-integration.md` already lands on the right design
rule:

- `zort` core stays semantic
- compiler compatibility lives in a dedicated compatibility layer
- raw OCaml-shaped ABI details belong in that layer, not in the semantic core

That aligns well with what the backend analysis shows.

The current OCaml native backend is too raw-ABI-shaped to target a purely
handle-oriented runtime API directly.

So `raml` has to choose between two broad strategies:

1. keep enough of the OCaml-style compiler/runtime contract to reuse large
   parts of the existing backend shape
2. define a new backend contract aimed at `zort`, then generate code for that
   contract directly

Either can work.
What will not work is pretending the existing backend contract is high-level
when it clearly is not.

## 3. The Cleanest Near-Term Reading

Given `zort`'s backlog and current focus, the cleanest near-term interpretation
is:

- do not port all of `asmcomp` blindly
- do not force the semantic `zort` core to become "the OCaml runtime"
- instead, make the compiler-facing compatibility surface explicit and staged

### First native-only layer after `Core_ir`

The native path should first lower directly from `Core_ir` into a native
compatibility layer or `NIR`, not through a speculative shared post-`Core`
layer.

That native-only layer should make explicit:

- raw compiler-value codecs and object/header conventions still required at the
  compiler/runtime boundary
- allocation, poll/safepoint, barrier, and root-update entrypoints
- unit-entry symbols, metadata tables, and linker-visible helper boundaries
- one locked target profile's calling convention and object-format constraints

Only after that layer should the pipeline diverge further into:

- machine-independent native IR choices
- target instruction selection
- register allocation
- assembler or binary emission

That also matches the current `zort` backlog items:

- lock one target triple and backend
- add a dedicated native-compiler compatibility shim
- implement startup and metadata ingestion
- implement raw compiler value codecs
- implement allocation and poll ABI
- implement barrier and root-update entrypoints

## 4. A Sensible `raml` Staging Plan

Step 1 is this manual.

A practical next staging plan would be:

### Phase 2: freeze the backend seam

Decide explicitly:

- what replaces `Lambda.program`
- what replaces the middle-end output contract
- whether `raml` keeps a Cmm-like IR, a Mach-like IR, both, or neither
- which parts of the OCaml runtime object model remain visible to the backend

### Phase 3: one no-allocation target path

Pick one concrete target triple and support only:

- integer/control flow
- calls and returns
- module entry
- no-allocation smoke programs

This keeps the first proof small.

### Phase 4: allocation, poll, and barriers

Add:

- tuple and string allocation
- poll insertion or an equivalent safepoint discipline
- field initialization versus mutation rules
- root metadata handling

### Phase 5: external primitives

Add:

- direct symbol calls
- argument/result boundary codecs
- a tiny primitive set

### Phase 6: exceptions and effects

Only after the lower-level compatibility seam is proven should `raml` bring
over:

- raises and catches
- callbacks
- perform/resume/reperform/runstack behavior

## 5. What Not To Do Early

The analysis also makes a few anti-goals obvious.

### Do not start with every target

`asmcomp` itself is split by target for a reason.
`zort` already says to lock one target first.

### Do not smear raw ABI concerns into `zort` core

The current backend assumes raw values, headers, closure layouts, and metadata
symbols.
Those belong in a compatibility boundary if `zort` wants to stay semantic.

### Do not pretend match compilation and recursive-value lowering are backend-
minor details

Those transformations happen early today and affect runtime shape.
If `raml` moves them, that should be an explicit design choice.

## 6. The Most Important Conclusion

The existing OCaml native backend is not just "a code emitter".

It is a stack of layers that jointly define:

- frontend lowering strategy
- runtime-facing IR semantics
- object layout
- GC-root typing
- target ABI
- linker-visible helper symbols

For `compiler/raml`, the main architectural task is therefore:

make those contracts explicit again, but in a form that can target `zort`
without forcing `zort` itself to become a thin clone of the OCaml runtime.
