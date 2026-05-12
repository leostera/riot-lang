# Strings, Bytes, Arrays, and Float Arrays

## Source anchors

- `vendor/ocaml/runtime/str.c`
- `vendor/ocaml/runtime/array.c`
- `vendor/ocaml/runtime/alloc.c`
- `vendor/ocaml/runtime/caml/mlvalues.h`

## Strings and bytes

- Strings and bytes share the same runtime representation: `String_tag`.
- Logical string length is not just `wosize * word_size`.
- The runtime stores the amount of tail padding in the last byte of the block, and `caml_string_length` subtracts it from the raw byte capacity.
- The last usable byte before padding must be `0`, which is asserted in debug checks.
- A string is “C-safe” only if `strlen(String_val(s)) == caml_string_length(s)`, meaning there is no interior NUL byte.

## Element access

- `caml_string_get` / `caml_bytes_get` bounds-check and return an OCaml int.
- `caml_bytes_set` mutates in place and bounds-checks.
- 16/32/64-bit get/set helpers use platform endianness rules encoded in the runtime.
- Out-of-bounds access raises the array bound exception via `caml_array_bound_error`.

## Array representation

- Non-float arrays are ordinary tag-`0` blocks.
- `caml_array_length` returns:
  - `Wosize_val(array)` for ordinary arrays
  - `Wosize_val(array) / Double_wosize` for flat float arrays
- Generic `caml_array_get` / `set` dispatch to floatarray-specific helpers when the block carries `Double_array_tag` under `FLAT_FLOAT_ARRAY`.

## Float arrays

- Float arrays are flat `Double_array_tag` blocks when flat-float arrays are enabled.
- Individual float reads allocate boxed `Double_tag` results.
- Zero-length float arrays return `Atom(0)`.
- Unsafe floatarray setters do not go through `caml_modify`; their memory-model story is called out as a TODO in the runtime itself.

## `Array.make` behavior

- `caml_uniform_array_make`:
  - returns `Atom(0)` for size `0`
  - uses minor allocation for small arrays
  - for large major arrays, if the initial value is a young block, forces a minor GC first so the array can be filled without creating many major-to-minor references
- `caml_array_make` picks floatarray versus uniform array based on the runtime representation of the initializer.

## zort takeaways

- OCaml strings are byte buffers with a length encoding trick, not plain `len + '\0'`.
- Bytes mutability is a convention above the representation layer.
- If zort wants simpler string rules, it should still decide whether it wants:
  - C-compatible sentinel storage
  - interior-NUL awareness
  - floatarray flattening
  - boxed-vs-flat float semantics for generic array operations

## zort language surface notes

- zort keeps a simpler string/bytes rule than OCaml:
  - logical length is stored explicitly,
  - payload storage is `len + 1` with a sentinel `0`,
  - bytes and strings share the same semantic runtime representation.
- The public semantic surface now exposes explicit bytes aliases:
  - `allocBytes`
  - `bytesLength`
  - `bytesSlice`
  - `setBytes`
- This is an intentional design choice:
  - bytes mutability stays a surface-level convention,
  - the core runtime does not introduce a separate bytes object kind.
