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

## The Main Conclusion

Grain strengthens the case for keeping wasm separate from native until a real
shared post-`Core_ir` layer proves itself.

The overlap between `raml-native` and `raml-wasm` is real, but it is mostly:

- pass structure
- ownership boundaries
- summary-artifact design
- backend-owned semantic analyses

It is not a strong argument for reviving one shared `ZIR` today.
