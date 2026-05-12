# Raml Native Backend Strategy

This document turns the `asmcomp` analysis into a concrete recommendation for
`compiler/raml`.

The goal is not to clone OCaml's native backend.

The goal is to decide:

- which seams from `vendor/ocaml/asmcomp` are worth preserving,
- which ones are too OCaml-runtime-shaped to reuse directly,
- and what final code-generation target `raml` should choose first.

## 1. What `asmcomp` Actually Gives Us

The useful lesson from `vendor/ocaml/asmcomp` is not "emit assembly like
OCaml".

The useful lesson is the shape of the backend stack:

1. a machine-independent native IR with explicit runtime commitments
2. instruction selection into a pseudo-machine IR
3. explicit polling/safepoint insertion
4. local optimization passes on that pseudo-machine IR
5. liveness, spilling, splitting, and register allocation
6. linearization into a flat low-level IR
7. post-linear scheduling
8. stack-frame analysis
9. emission and assembly/object generation
10. linking, archiving, and packaging as explicit later phases

That stack is visible in:

- `vendor/ocaml/asmcomp/asmgen.ml`
- `vendor/ocaml/asmcomp/cmm.mli`
- `vendor/ocaml/asmcomp/selectgen.mli`
- `vendor/ocaml/asmcomp/mach.mli`
- `vendor/ocaml/asmcomp/linear.mli`
- `vendor/ocaml/asmcomp/stackframegen.mli`
- `vendor/ocaml/asmcomp/asmlink.mli`

The key idea to borrow is not the exact IR names.

The key idea to borrow is:

- preserve multiple late seams,
- keep target-generic work alive as long as possible,
- and keep the final emitter/linker surface explicit.

## 2. What `raml` Should Borrow

`raml` should keep these ideas.

### Borrow the staged backend shape

After `Core_ir`, the native path should still have multiple layers:

- `NIR` as the first native-only compatibility/runtime-oriented IR
- `MIR` as the machine-oriented native IR
- `LIR` as the final flat pre-emission IR
- target emission
- artifact/link surface

Do not jump from `Core_ir` straight to textual assembly.

### Borrow explicit safepoint and frame analysis passes

Polling and frame requirements are not emitter trivia.

They should remain named passes because they affect:

- control flow,
- GC correctness,
- exception behavior,
- and runtime metadata.

### Borrow a Linear-like final IR boundary

`asmcomp`'s restart seam at saved `Linear` is a good design.

`raml` should likely keep one final flat IR before emission for:

- debugging,
- caching,
- testing,
- alternate emitters,
- and future experimentation with object emission versus text assembly.

### Borrow the target split

The target backend is not one module.

`asmcomp` is right to keep separate ownership for:

- ISA description
- calling convention
- selection hooks
- reload legality
- scheduling facts
- stack-frame policy
- emission syntax

`raml` should keep that split.

## 3. What `raml` Should Not Borrow Directly

`asmcomp`'s exact IR content is too tied to the existing OCaml runtime ABI.

`raml` should not copy directly:

- the exact Cmm primitive vocabulary
- the exact object-layout helpers in `cmm_helpers`
- the exact metadata symbol set
- the assumption that the backend's raw value model is identical to OCaml's
  forever

Those belong in the native backend's own late IR family above `zort`, not in
the shared compiler center and not in the semantic `zort` core.

So the lesson is:

- borrow the pass boundaries,
- do not blindly borrow the runtime encoding vocabulary.

## 4. Recommended `raml` Native Stack

The current best native stack for `raml` is:

```text
Typ Semantic Tree
  -> Raml Core IR
  -> shared passes
  -> NIR
  -> MIR
  -> LIR
  -> target emitter
  -> assembler/object writer
  -> linker/archive/packager
```

This native path should not infer the emit target from the machine running the
compiler.

Native backend dispatch should always be read as:

- `host` triple: where the compiler itself is running
- `target` triple: what object format, ABI, and backend family the compiler is
  producing

So `host = aarch64-apple-darwin` and `target = x86_64-unknown-linux-gnu` is a
normal cross-compile, and `target = js-unknown-ecma` should dispatch out of the
native path entirely and into the JS backend.

### `NIR`

This should be the first native-only layer after `Core_ir`.

It should own:

- raw value codec decisions still required by the compiler/runtime boundary
- allocation and poll entrypoints
- barrier and root-update entrypoints
- metadata tables and unit-entry conventions
- one locked target profile's ABI assumptions

This is where `raml` becomes native-runtime-shaped.

### `MIR`

This should play the role that Mach plays structurally:

- pseudo-instructions
- explicit calling convention placement
- target-selected operations
- enough structure for liveness, reload, splitting, and regalloc

### `LIR`

This should be the last structured IR before emit.

It should be:

- flat
- label-based
- close to final instruction order
- suitable for snapshots and restart-at-emit workflows

## 5. Decision: Do Not Start With LLVM

Recommendation:

- do not use LLVM as the first native backend target for `raml`

LLVM is attractive for:

- target coverage
- register allocation
- object generation
- mature assemblers and linkers

But it is a poor first fit for the problems `raml` actually has to solve next.

The immediate hard problems are:

- exact compiler/runtime boundary ownership
- safepoints and polling discipline
- root-sensitive value classes
- frame metadata
- custom runtime helper conventions
- target-locked startup and allocation ABI

Those are not the parts LLVM solves for us.

LLVM would force `raml` to solve the hard runtime contract first anyway, then
translate that contract into LLVM's model, while also fighting:

- tailcall constraints
- custom calling conventions
- stack-map/GC integration choices
- exception/control-transfer mismatches
- lowered code that becomes harder to reason about against the native runtime
  contract

So LLVM adds another semantic translation problem before the first target is
even proven.

That is the wrong tradeoff for v1.

LLVM can still be a later experiment if `raml` reaches a stable `LIR`
and wants another emitter path.

It should not be the first native path.

## 6. Decision: Zig Is Not The Right Compilation Target

Recommendation:

- do not treat Zig as the native compilation target for `raml`

Zig is useful in this repository as:

- host/runtime implementation language
- build and packaging tooling
- compatibility-shim toolchain

But as a compiler target it does not remove the core backend problem.

If `raml` targets Zig source, it still has to encode:

- raw values
- calls
- safepoints
- metadata tables
- stack behavior
- runtime helper boundaries

and then hope Zig's optimizer/codegen preserves the required low-level
properties.

That is weaker than owning the low-level native pipeline directly.

So Zig is a good tool around the backend.
It is not the right backend target.

## 7. Recommended First Codegen Choice

Recommendation:

- lock one target triple first
- lower into a `Linear IR`
- emit assembly directly for that target
- use the system assembler/linker, or a direct object writer later if needed

For this repository, the obvious first locked target remains:

- `aarch64-apple-darwin`

because that already matches the current `zort` compatibility focus in:

- `zort/BACKLOG.md`
- `zort/spec/compiler-runtime-integration.md`

This choice keeps the first native backend honest:

- one target
- one ABI
- one emitter
- one runtime-compat contract

That is exactly the scale the project needs right now.

## 8. Practical Near-Term Plan

The next useful implementation plan is:

1. define `src/native/` around IR and pass seams, not emitters first
2. freeze `Native Compat IR`
3. define a small Cmm-like `Native IR`
4. define a small Mach-like `Machine IR`
5. define a restartable `Linear IR`
6. implement one `aarch64-apple-darwin` emitter
7. only after that consider whether a second emitter path is worth adding

The big decision, then, is:

- preserve `asmcomp`'s staged backend shape
- do not adopt LLVM first
- do not target Zig source
- emit direct native assembly first from a `Linear IR`

That is the most defensible first native path for `raml`.
