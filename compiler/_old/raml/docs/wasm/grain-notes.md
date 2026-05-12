# Grain Wasm Notes

These notes are about one specific question:

what does Grain's wasm backend teach us about the shape `raml-wasm` should
have, and how much of that shape overlaps with `raml-native`?

The short answer is:

- Grain is useful evidence for a wasm-first backend architecture
- it is not a good source for a shared `raml` IR
- the overlap with native is real, but it sits higher than machine code and
  lower than `Core_ir`

## What Grain Actually Does

Grain has a dedicated wasm pipeline. The front half is:

- typed program
- ANF in `middle_end/anftree.re`
- repeated ANF analyses and optimization passes

Then it lowers into a wasm-oriented low-level IR:

- `codegen/mashtree.re`

That IR is not generic. It already knows about:

- wasm primitive types
- wasm operations
- heap layout
- function tables
- globals and imports
- runtime allocation and garbage collection concerns

After that, Grain links module objects at the `mashtree` level in
`codegen/linkedtree.re`, then builds a Binaryen module in
`codegen/compcore.re`, and finally emits `.wasm` and optional `.wat` in
`codegen/emitmod.re`.

So the real Grain stack is:

`typed -> ANF -> optimized ANF -> mashtree -> linkedtree -> Binaryen module -> wasm`

## What Is Worth Borrowing

Three things are especially useful for `raml-wasm`.

First, wasm gets its own low-level IR before emission.

Grain does not try to emit wasm directly from a high-level shared tree. It has
`mashtree`, which is already close enough to WebAssembly semantics that codegen
becomes mostly a structural translation into Binaryen.

Second, separate compilation is part of the backend contract.

Grain emits object files that carry both signature information and a low-level
program representation. Linking happens before final wasm generation, not as a
late text-level step.

Third, runtime-sensitive transformations are explicit passes.

A good example is `codegen/garbage_collection.re`, which rewrites the low-level
IR to insert refcount operations. That work is not hidden inside the emitter.

## More Specific Things Worth Borrowing

There are four more concrete lessons in Grain that are worth carrying forward.

### A real object format, not just a pretty snapshot

Grain object files are not vague compiler cache blobs.

`emitmod.re` writes a real versioned object layout:

- magic bytes
- compiler version
- signature length and signature payload
- low-level code payload

That is a good lesson for `raml-wasm`.

If we want separate compilation, we should give wasm its own real object
artifact early instead of pretending the summary can stay an ad hoc JSON shape
forever. The exact binary format does not matter yet, but the boundary does.

### Linking owns symbol resolution

`linkedtree.re` does not just concatenate low-level modules. It resolves
module-local exports and imports through link-time tables, scopes internal
symbols by dependency id, and rewrites imports against those resolutions.

That is useful because it tells us where wasm name hygiene actually belongs:

- not in `raml-core`
- not in the emitter
- in a wasm-owned link/object layer

So `raml-wasm` should expect a real linked-program stage after object loading,
even if the first version is very small.

### Late optimization should be feature-aware

`optimize_mod.re` is not just "run Binaryen".

It chooses late optimization passes based on:

- optimize and shrink levels
- enabled wasm features like GC, strings, and multivalue
- closed-world assumptions

That is a good reminder that Binaryen belongs late and target-profile-aware.
`raml-wasm` should not treat Binaryen as the main compiler IR. It should treat
it as a late optimizer/builder whose pass policy depends on the actual wasm
feature set we are targeting.

### Link time can own metadata sections

The printing metadata docs are also useful. Grain builds type metadata into a
linked wasm data section, not as scattered local compiler hacks.

That matters because it shows another category of backend product:

- the wasm module itself
- the object/summary artifacts that feed linking
- linked data sections or sidecar tables for runtime services

So if `raml-wasm` later wants reflection tables, effect metadata, string
literal segments, or runtime manifests, link time is a reasonable ownership
boundary for that work.

## What Not To Copy Too Literally

A few Grain choices are useful as evidence but still wrong for us to copy
directly.

- Grain's ANF entrypoint is a good reminder that wasm wants a linear,
  effect-explicit tree before low-level lowering. It is not evidence that
  `raml-core` should grow Grain-style ANF as the one true shared IR.
- Grain's refcounting and bespoke allocator are coherent for Grain's runtime,
  but they are not a reason for `raml-wasm` to commit to the same memory story
  before we decide whether Riot wants Wasm GC, a custom runtime heap, or both.
- Grain's Binaryen-heavy backend is a good late-stage strategy. It is not a
  reason to let Binaryen types or passes leak upward into `WIR`.

## What Not To Borrow

Grain is wasm-first, so wasm concepts leak very early into the language and
typed layers.

You can see wasm primitive types and operations in:

- `parsing/parsetree.re`
- `typed/typedtree.re`
- `middle_end/anftree.re`

That is exactly the wrong direction for `raml` if `raml` is supposed to stay a
multi-backend compiler. `raml-core` should not become aware of wasm opcodes,
Binaryen feature flags, or wasm-specific primitive families just because the
wasm backend needs them.

## The Native And Wasm Overlap

There is overlap, but it is not where `asmcomp`-style native code lives.

The overlap is in backend-owned semantic lowering, not in target mechanics.

Things native and wasm probably can share in spirit, and maybe eventually in
code if the shape lines up:

- closure and direct-vs-indirect call classification
- arity and partial-application decisions
- module initialization order
- export/import summary artifacts
- runtime-helper classification
- explicit lowering of backend obligations into passes instead of emitters
- conservative dead-code and copy cleanup on backend-owned low-level IRs

Things they should not share right now:

- frame layout
- register or home assignment
- ABI rules
- object format and linker behavior
- wasm tables
- wasm feature negotiation
- Binaryen integration
- native assembly emission

So the honest overlap is:

`Core_ir`
-> backend-specific semantic lowering
-> backend-specific low-level IR

where the semantic lowering stages may end up looking similar, but the low-level
IR families diverge quickly.

## What This Suggests For `raml`

The cleanest current decomposition still looks like:

- `raml-core`
  owns shared frontend lowering and `Core_ir`
- `raml-native`
  owns `NIR -> MIR -> LIR -> emitter -> linker`
- `raml-wasm`
  should grow its own wasm-oriented low-level IR and artifact story

If `raml-wasm` follows the Grain lesson without inheriting Grain's wasm-first
bias, the likely shape is something like:

`Core_ir -> WIR -> wasm passes -> Binaryen or wasm emitter -> wasm object/module artifacts`

That `WIR` would be the right place for:

- wasm imports and exports
- table/function-ref decisions
- heap or GC representation decisions
- runtime helper insertion
- module summary artifacts for separate compilation

It would not be the right place for native homes, native frame layout, or
Mach-O details.

The object/link side probably wants one more explicit step than that:

`Core_ir -> WIR -> wasm passes -> wasm object artifact -> linked wasm program -> Binaryen or emitter`

That is closer to Grain's real shape, and it lines up with the way native owns
emission and linking as separate concerns.

## The Main Conclusion

Grain strengthens the case for keeping wasm separate from native until a real
shared post-`Core_ir` layer proves itself.

The overlap between `raml-native` and `raml-wasm` is real, but it is mostly:

- pass structure
- ownership boundaries
- summary-artifact design
- backend-owned semantic analyses

It is not a strong argument for reviving one shared `ZIR` today.

The most concrete design pressure Grain adds is this:

`raml-wasm` probably wants a real object artifact and a real linked-program
stage sooner than it wants a real wasm emitter.
