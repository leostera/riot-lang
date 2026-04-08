# Custom Blocks, Bigarrays, and Boxed Numerics

## Source anchors

- `vendor/ocaml/runtime/caml/custom.h`
- `vendor/ocaml/runtime/custom.c`
- `vendor/ocaml/runtime/bigarray.c`
- `vendor/ocaml/runtime/ints.c`

## Custom block model

- `Custom_tag` blocks are raw blocks whose first word points to a `custom_operations` table.
- The GC never scans custom payload bytes for `value`s.
- Custom payloads therefore must not contain OCaml heap pointers unless some other mechanism roots them.

## Operations table

- A custom operations table may define:
  - `finalize`
  - `compare`
  - `hash`
  - `serialize`
  - `deserialize`
  - `compare_ext`
  - fixed-length metadata
- Tables are registered globally by identifier.
- Deserialization depends on looking up the identifier at input time.

## GC/resource interaction

- `caml_alloc_custom` and `caml_alloc_custom_mem` take resource accounting parameters.
- Small/lightweight custom blocks may be allocated in the minor heap.
- Large or expensive custom blocks go directly to the major heap.
- Custom resource usage feeds GC pacing through the custom major/minor ratios and max-minor-bytes threshold.

## Standard custom-backed values

- `Int32.t`, `Int64.t`, and `Nativeint.t` are custom blocks, not immediates.
- Their custom ops define compare/hash/serialize/deserialize behavior.
- `Obj.new_block Custom_tag` is rejected because an uninitialized custom-ops pointer would make hashing/finalization/serialization unsafe.

## Bigarrays

- Bigarrays are custom blocks using the `_bigarr02` operations table.
- Bigarray payload may point to external memory.
- If `data == NULL`, the runtime allocates the backing storage itself.
- Bigarray data must not point into the OCaml heap.
- Dimension metadata may be read from OCaml memory during allocation.
- Bigarray element size depends on the kind:
  - floats, ints, native ints, complex values, chars, float16, etc
- Hashing, comparison, serialization, and finalization are delegated through custom ops.

## zort takeaways

- “Boxed numerics” in OCaml are not a separate mechanism from custom blocks; they are a specific use of the custom-block machinery.
- If zort wants stronger type safety, separating:
  - boxed machine numbers
  - foreign resource handles
  - externally-backed arrays
  is a good design move, but the spec baseline is that OCaml lumps them into the same raw custom-block substrate.
