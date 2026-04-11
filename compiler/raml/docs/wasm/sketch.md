# Raml Wasm Sketch

This is the concrete package sketch for `raml-wasm`.

The intent is simple:

- keep wasm isolated from native
- give wasm its own low-level boundary early
- make imports and artifact seams explicit before codegen exists

## Package Shape

The first useful package shape is:

```text
compiler/raml-wasm/src/
  backend.ml
  wir/
    types.ml
    runtime_imports.ml
    artifacts.ml
    lowering.ml
```

That gives the backend four distinct responsibilities:

- `Wir.Types`
  owns the wasm-oriented IR
- `Wir.Runtime_imports`
  owns runtime and host import classification
- `Wir.Artifacts`
  owns per-module wasm summaries
- `Wir.Lowering`
  owns `Core_ir -> WIR`

## First Pipeline

The first real wasm pipeline should be:

```text
Core_ir
  -> WIR
  -> explicit wasm passes
  -> wasm artifact / object summary
  -> Binaryen or wasm encoder lowering
  -> final wasm module + sidecars
```

Right now only the first two seams need to exist in code:

- `Core_ir -> WIR`
- `WIR -> module summary`

That is enough to stop talking about wasm as a single opaque "backend stub".

## Why `WIR` Exists

`WIR` is where wasm-specific commitments start, but only the ones that really
belong to wasm:

- explicit imports
- explicit distinction between top-level functions, globals, and init
- primitive classification for pure ops versus runtime or host helpers
- a place to hang later wasm-specific passes

It is deliberately not:

- a native-like machine IR
- a Binaryen AST
- a shared `ZIR` revival

## What Overlaps With Native

The overlap with `raml-native` is real, but it sits above machine details.

The overlap is in backend discipline:

- explicit lowering out of `Core_ir`
- explicit runtime-helper classification
- explicit artifact boundaries
- explicit backend-owned passes

The overlap is not in low-level IR design.

Native needs:

- homes
- frame layout
- call-clobber and ABI rules
- assembly and linker details

Wasm needs:

- imports and exports
- table or function-ref decisions
- GC/reference representation
- module/object summary artifacts
- loader or sidecar packaging

So both backends should look similar in ownership, but not in IR shape.

## What Comes Next

Once `WIR` exists, the next wasm-owned questions become concrete:

1. Which `Core_ir` constructs need closure conversion before wasm codegen?
2. Which runtime services become imports, and which become real wasm helpers?
3. Do we target Binaryen late, or write a Riot-owned wasm encoder path first?
4. What does a wasm object artifact need for separate compilation?

Those are the right next questions.
They are much cleaner than trying to reuse native IR or pretending wasm is just
"another emitter".
