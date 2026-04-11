# Raml Wasm Sketch

This is the concrete package sketch for `raml-wasm`.

The intent is simple:

- keep wasm isolated from native
- give wasm its own low-level boundary early
- make imports and artifact seams explicit before codegen gets deep

## Package Shape

The first useful package shape is:

```text
compiler/raml-wasm/src/
  backend.ml
  codegen.ml
  wir/
    types.ml
    runtime_imports.ml
    artifacts.ml
    lowering.ml
    passes/
      normalize.ml
      collect_imports.ml
```

That gives the backend four distinct responsibilities:

- `Wir.Types`
  owns the wasm-oriented IR
- `Wir.Runtime_imports`
  owns runtime and host import classification
- `Wir.Artifacts`
  owns per-module wasm summaries, object artifacts, and the first linked-program
  shape
- `Wir.Lowering`
  owns `Core_ir -> WIR` and threads explicit wasm passes
- `Codegen`
  owns the first executable wasm slice: a direct binary emitter over the linked
  program plus a Node runner sidecar
- `Wir.Passes.Normalize`
  owns local structural cleanup on lowered `WIR`
- `Wir.Passes.Collect_imports`
  owns runtime and host import discovery

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

Right now the first useful seams in code are:

- `Core_ir -> WIR`
- `WIR -> normalize -> plan_runtime -> dead_code -> collect_imports`
- `WIR -> object artifact -> linked program`
- `linked program -> wasm binary + node runner`

That is enough to stop talking about wasm as a single opaque "backend stub".

The current executable slice is intentionally narrow. It is good enough for
programs whose top-level init lowers to supported runtime print calls and static
constants, and it fails explicitly outside that slice.

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
3. What extra data must the object and linked-program layers carry for separate compilation?
4. When do we outgrow the current direct encoder and introduce Binaryen or a
   richer Riot-owned wasm emitter?

Those are the right next questions.
They are much cleaner than trying to reuse native IR or pretending wasm is just
"another emitter".
