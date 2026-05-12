# Marshaling and Code Fragments

## Source anchors

- `vendor/ocaml/runtime/extern.c`
- `vendor/ocaml/runtime/intern.c`
- `vendor/ocaml/runtime/codefrag.c`

## Marshal flags

- The runtime marshaler recognizes these flags:
  - `NO_SHARING`: do not preserve object sharing
  - `CLOSURES`: allow marshaling closures/code pointers
  - `COMPAT_32`: reject values that a 32-bit runtime could not read back
  - `COMPRESSED`: request compressed output when available

## Output behavior

- By default, marshaling preserves sharing using an object-position table.
- With `NO_SHARING`, repeated references may duplicate structure.
- `COMPAT_32` rejects values that overflow 32-bit limits, including some large ints/arrays/strings/float arrays.
- Marshaling a closure requires `CLOSURES`; otherwise it raises `Invalid_argument("output_value: functional value")`.
- Marshaling private functions, continuations, or some abstract/out-of-heap values is rejected.
- Abstract blocks cannot be marshaled.
- Custom blocks can be marshaled only if their custom ops provide the necessary callbacks.

## Input behavior

- Input validates read bounds and object counts.
- Truncated or malformed input fails with `Failure` / invalid-read style errors.
- Unknown custom block identifiers fail at deserialization time.
- Wrong custom-block kind or custom deserialize errors are surfaced as input failures.
- Compressed input is transparently decompressed if support is available.

## Code fragments

- The runtime maintains a global table of code fragments keyed by:
  - program counter range
  - fragment number
  - optional MD5 digest
- Digests may be:
  - ignored
  - computed immediately
  - computed lazily
  - provided externally
- Closure marshaling and code-pointer resolution depend on this table.

## zort takeaways

- Marshaling closures is inseparable from code-fragment identity.
- If zort does not plan to preserve OCaml `Marshal` behavior for closures/custom blocks, that should be an explicit non-goal.
- A replacement runtime can profitably split:
  - pure data serialization
  - code serialization / code identity
  instead of treating them as one feature bucket.
