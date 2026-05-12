# Raml Multi-Backend Compatibility Notes For JS

This document records what the current Melange JS backend implies for
`compiler/raml` as a multi-backend compiler targeting:

- native
- JavaScript
- wasm

The main lesson is simple:

the JavaScript backend needs a lot of real compiler work, but most of that work
should not live in a JS-colored shared IR.

## 1. What The Compiler Still Has To Do For JS

Even with a runtime library, the compiler still needs to own:

- module dependency discovery and initialization ordering
- closure and arity analysis
- recursive value lowering
- recursive module lowering
- control-flow lowering for exceptions, loops, and switches
- top-level grouping and export shaping
- data-constructor and field-access lowering
- cross-module summary generation
- import/export path materialization
- final JS AST emission and cleanup

So the JS backend is not "just an emitter".

It still needs serious backend logic.

## 2. The Safe Shared Compiler Responsibilities

These responsibilities should likely stay shared across backends:

- semantic lowering out of the frontend language
- explicit module graph and init-order modeling
- explicit exports
- closure and apply semantics
- recursive binding semantics
- abstract data-constructor semantics
- exception semantics
- typed foreign-declaration metadata
- source-span and origin tracking

Those are not JS-specific.

They are backend-facing language semantics.

## 3. The JS-Only Responsibilities

These should belong only to the JS backend:

- CommonJS versus ESM emission
- package-relative import path generation
- default-import versus namespace-import decisions
- JS runtime helper selection
- JS-specific option/null/undefined representation
- JS object-literal and property-access FFI lowering
- raw JS escapes
- dynamic `import()` lowering
- JS tree-shaking and dead-declaration cleanup

If these leak into the shared middle, native and wasm will inherit design
constraints they do not need.

## 4. A Better Layer Split For `raml`

The current Melange code suggests a useful replacement stack.

### Layer 1: frontend semantic IR

This layer should capture the language as `raml` wants to understand it after
parse and typing.

It should still be language-facing, not runtime-facing.

### Layer 2: backend-neutral executable IR

This layer should make backend work explicit:

- closures
- apply conventions
- module init ordering
- exports
- recursive values
- exceptions
- data constructors

But it should still avoid target representation choices.

### Layer 3: backend-specific lowering

This is where JS, native, and wasm diverge.

For JS, this layer chooses:

- runtime value layout
- import/export ABI
- JS FFI semantics
- JS module system

For native and wasm, the same shared executable IR can lower to different late
IR stacks.

## 5. The Shared IR Needs More Than A Tiny Core

One design risk would be over-correcting and inventing a tiny, pretty,
backend-neutral lambda calculus that is too abstract to drive real codegen.

Melange is useful here because it shows the shared middle still needs real
backend facts such as:

- arity
- purity or side-effect information
- module-global references
- initialization groups
- recursive update strategy boundaries
- exported symbol tables

The right move is not "tiny core, solve later".

The right move is:

- rich shared semantics
- late backend-specific representation

## 6. What To Borrow From Melange

These are the parts worth reusing as design ideas.

### Distinct backend stages

Do not collapse the backend into one pass.

### Cross-module summary artifact

Keep a `.cmj`-like summary artifact for downstream compilation.

### Explicit package/path layer

Import pathing and module system should be first-class data.

### Explicit runtime contract docs

Document runtime ABI expectations as part of backend work, not as folklore.

## 7. What To Avoid Borrowing

These are the parts most worth replacing.

### JS-colored shared IR

Do not make the first backend IR carry `Pjs_*` primitives and JS wrappers if it
is meant to be shared with native and wasm.

### Global frontend hook mutation as backend API

Try to move the shared frontend/backend boundary later and make it explicit
instead of relying on global compiler patch points.

### Stringly FFI payloads

The internal `[@mel.internal.ffi "..."]` path is pragmatic, but `raml` should
prefer typed foreign metadata after parsing.

## 8. A Practical First JS Slice For `raml`

A sane first slice would be:

1. one package, one module system
2. shared executable IR with explicit modules, exports, closures, and data
3. JS-lowered IR for runtime layout and import/export materialization
4. `.cmj`-like summary without cross-module inlining at first
5. documented runtime representation for:
   - modules
   - options
   - records
   - ordinary variants
   - exceptions
6. typed JS externals without raw-JS escapes as the default path

That would already exercise the real backend seams without forcing native and
wasm to adopt JS semantics.

## 9. Questions `raml` Needs To Answer Explicitly

Melange's source makes these questions unavoidable.

### Where does pattern-match lowering happen?

Before the shared executable IR, or inside it?

### Where does recursive-value lowering happen?

In a shared pass, or separately per backend runtime?

### Where is module-init ordering decided?

It must be explicit somewhere before emission.

### What does the summary artifact carry?

At minimum:

- exports
- dependency/package info
- init-order facts
- representation-relevant backend facts for downstream codegen

### How is FFI represented before backend lowering?

It should probably be a typed, backend-neutral declaration with backend-
specific lowering rules.

## 10. The Main Design Extraction

The Melange JS backend is a good requirements document for `raml`.

It is not a good shared IR.

The core work for `compiler/raml` is to keep the backend stages explicit while
moving the backend-neutral/backend-specific boundary later than Melange
currently does.
