# Raml Cmm Notes

This document records what `vendor/ocaml/asmcomp` does at the machine-
independent native IR layer.

This is the point where the backend stops talking in frontend Lambda terms and
starts talking in explicit runtime object-model terms.

## 1. What Cmm Represents

`asmcomp/cmm.mli` defines the second intermediate language used by the native
backend.

The most important type is `machtype_component`:

- `Val`
- `Addr`
- `Int`
- `Float`

This split does two jobs at once:

- register-class selection
- GC-root tracking

The crucial distinction is between:

- `Val`
  safe OCaml values and GC roots
- `Addr`
  derived heap pointers that must never stay live across allocation or calls
- `Int`
  non-root machine integers or non-heap pointers
- `Float`
  FP values

That means the Cmm layer already encodes GC liveness policy, not just machine
types.

## 2. Cmm Is Where Runtime Operations Become Concrete

The Cmm `operation` set includes:

- direct application and external calls
- loads and stores with chunk, mutability, and atomicity
- allocation
- integer and float arithmetic
- raises
- bounds checks
- opaque identity
- domain-local-state access
- polling

The expression language includes:

- lets and mutable lets
- phantom lets
- assignment
- tuples
- primitive operations
- sequence
- conditionals
- switches
- catch/exit continuations
- try/with
- explicit return-address fetch

This is already a backend control-flow IR, not a simple expression tree.

## 3. `cmmgen` Lowers Closed Lambda To Cmm

`cmmgen.mli` exposes:

- `compunit :
    Clambda.ulambda
    * Clambda.preallocated_block list
    * Clambda.preallocated_constant list
    -> Cmm.phrase list`

So `asmcomp` does not lower the frontend `Lambda.lambda` directly.
It lowers a closed Lambda/Clambda form plus preallocated data.

### Translation environment

`cmmgen.ml` tracks:

- unboxed identifiers
- mutable identifiers
- catch-argument notifications
- the current closure environment parameter

That environment matters for:

- deciding whether a variable is boxed or unboxed
- deciding whether a variable is mutable
- knowing when loads from the current closure can be treated as immutable

## 4. Closure Layout Is Made Explicit Here

The `Uclosure` translation path in `cmmgen.ml` is one of the clearest backend
contracts in the whole system.

For closures with free variables, `cmmgen` lays out:

- closure headers
- code pointers
- closure-info words
- infix headers for later functions in a set
- then captured environment fields

The helper calls involved include:

- `alloc_closure_header`
- `alloc_closure_info`
- `alloc_infix_header`
- `make_alloc ... Obj.closure_tag ...`

There is also an explicit comment about why arbitrary closure-variable
expressions are safe:

- `make_alloc` evaluates and fills fields left-to-right
- it does not trigger a GC between allocation and field filling
- so metadata fields are written before the closure can be observed by GC

That is exactly the kind of low-level invariant a `zort` compatibility layer
must either preserve or replace with a new backend contract.

## 5. `cmm_helpers` Is The Runtime Object-Model Toolbox

`cmm_helpers.mli` is effectively the backend's runtime-layout DSL.

It exposes helpers for:

- block headers
- closure headers and closure-info words
- infix headers
- float, string, boxed-int, and float-array headers
- OCaml integer tagging and untagging
- block field loads and stores
- header, tag, and size extraction
- array indexing and array-length decoding
- boxing and unboxing floats and boxed integers
- exception raising
- generic apply/send/curry helper generation
- preallocated block and GC-root table emission

This file is where a lot of "what the runtime must look like" becomes literal.

## 6. Generic Apply/Send/Curry Functions Are Part Of The Backend Contract

`cmm_helpers.generic_functions` synthesizes helper functions for:

- generic apply
- generic send
- curry helpers

One comment is especially revealing:

- apply functions of arity 2 and 3 are always present in the main program
  because the runtime system needs them

That means the backend/runtime seam is not only:

- allocation
- barriers
- exceptions

It also includes well-known helper entrypoints used by compiler-generated
calling sequences.

## 7. Constants, Data, And Preallocated Blocks

`cmmgen` and `cmm_helpers` cooperate to emit:

- structured constants
- constant closures
- the unit entry function
- preallocated blocks
- a GC-roots table

`emit_preallocated_blocks` first builds a NULL-terminated GC-roots table and
then emits each preallocated block.

The comments note a no-naked-pointers concern:

- root-registered words must contain valid values
- preallocated block headers must be black

So even constant-data emission is entangled with GC policy.

## 8. Instrumentation Lives At Cmm

The backend can instrument Cmm with:

- AFL coverage
- ThreadSanitizer support

`cmmgen` inserts:

- `Afl_instrument.instrument_function`
- `Afl_instrument.instrument_initialiser`
- `Thread_sanitizer.instrument`

The TSan notes are especially useful because they explain that:

- function entry/exit instrumentation is required
- dynamic exception/effect/continuation transitions also need runtime support

This confirms that some compatibility work belongs above the semantic runtime
core but below "ordinary user primitives".

## 9. Cmm Invariants Matter

`cmm_invariants.mli` documents continuation-related invariants:

- every continuation use stays within handler scope
- exit argument counts match handler parameter counts
- a given continuation is declared in only one handler per function

These invariants are optionally checked with `-dcmm-invariants`, but the more
important point for `raml` is architectural:

the backend relies on explicit IR invariants and even documents them in a
checker module.

That pattern is worth copying.

## 10. What `compunit` Emits

`cmmgen.compunit` does not emit just one function.

It builds:

- an entry function named from `Compilenv.make_symbol (Some "entry")`
- translated constants
- all translated functions
- preallocated blocks and GC roots
- remaining constant data items

The entry function is also given reduced-code-size preferences because it is
often large and run only once.

That is a reminder that compilation policy can differ per generated artifact,
not just per source function.

## 11. Cmm-Level Runtime Assumptions That Matter For `zort`

From the backend's point of view, the runtime exposes at least these concepts:

- tagged immediates
- block headers and tag/size extraction
- closure headers and closure-info words
- infix pointers
- mutable versus immutable loads
- allocation with precise initialization order
- write barriers and initialization barriers
- generic apply/send/curry helpers
- symbol-addressable global data and preallocated roots
- external-call ABI names
- polling

If `zort` wants the existing compiler to target it directly, those contracts
must exist somewhere.

If `raml` wants to target `zort` without copying OCaml's raw ABI, then `raml`
needs a different machine-independent IR contract than Cmm currently assumes.

## 12. Design Pressure On `raml`

The current Cmm layer suggests a few concrete rules.

### Make the runtime object model explicit in one layer

Today that layer is Cmm plus `cmm_helpers`.
If `raml` chooses a different IR, it still needs one place where:

- headers
- tags
- boxing
- roots
- closures
- barriers

are no longer implicit.

### Treat GC-root typing as an IR concern

`Val` versus `Addr` is not cosmetic.
It is the difference between safe roots and invalid interior pointers across GC.

### Keep data emission and metadata emission inside the backend

Constants, GC roots, and helper-function synthesis are backend work today.
Pushing them out into ad hoc runtime code would weaken the compiler/runtime
contract instead of clarifying it.
