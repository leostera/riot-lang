# Numeric, String, and Array Primitives

## Source anchors

- `vendor/ocaml/runtime/floats.c`
- `vendor/ocaml/runtime/ints.c`
- `vendor/ocaml/runtime/str.c`
- `vendor/ocaml/runtime/array.c`

## Integer parsing and formatting

- Integer parsing accepts `_` separators.
- Base/sign handling follows the runtime parser rather than `strtol`:
  - base prefixes are recognized
  - the whole string must be consumed
  - overflow raises `Failure`
- Signed vs unsigned parsing ranges are enforced explicitly by bit width.
- Integer formatting rewrites the user format string with an architecture-specific suffix before printing.
- Overlong integer format strings raise `Invalid_argument("format_int: format too long")`.

## Float parsing and formatting

- Float parsing is locale-stable.
- The runtime initializes and uses the `"C"` numeric locale explicitly so third-party `setlocale` calls do not silently change OCaml float syntax.
- `float_of_string`:
  - strips `_`
  - rejects empty input
  - accepts hexadecimal floating-point syntax via a custom parser
  - otherwise uses locale-stable `strtod`
  - requires full-string consumption
  - raises `Failure("float_of_string")` on invalid input
- `format_float` also runs in the `"C"` numeric locale.
- On platforms with broken `printf` handling for infinities/NaNs, the runtime falls back to:
  - `"nan"`
  - `"inf"`
  - `"-inf"`

## Float arithmetic and classification

- `int_of_float` is a direct C cast/truncation.
- `float_of_int` boxes the integer as a double.
- Float comparisons split into two behaviors:
  - ordinary comparison operators (`=`, `<`, `<=`, etc.) use raw C floating semantics
  - `caml_float_compare` uses a total order where `NaN = NaN` and `NaN < x` for all non-NaN floats
- `classify_float` returns one of:
  - normal
  - subnormal
  - zero
  - infinite
  - NaN
- `signbit` is exposed directly.

## Strings and bytes

- String length is not `strlen`.
- The runtime stores a length/terminator invariant in the tail byte and reconstructs the logical length from block size plus that tail marker.
- A string is C-safe only if `strlen(String_val(s)) == caml_string_length(s)`.
- `String.create`/`Bytes.create` reject oversize allocations with `Invalid_argument`.
- `string_get` / `bytes_get` and `bytes_set` are bounds-checked and raise the array-bounds exception on invalid indexes.
- `get16/32/64` and `set16/32/64` also bounds-check and use host endianness when assembling/disassembling integers from bytes.
- String equality is structural byte equality.
- String comparison is lexicographic by bytes, then by length.

## Arrays and float arrays

- With flat float arrays enabled, float arrays are physically distinct from ordinary value arrays.
- `caml_array_length` returns:
  - word fields for ordinary arrays
  - float element count for flat float arrays
- Safe get/set operations dispatch on the physical representation and bounds-check.
- Unsafe get/set operations omit bounds checks.
- Zero-length float arrays are represented as `Atom(0)`.

## Array construction behavior

- `Array.make` chooses the float-array path when the initializer is a boxed float and flat float arrays are enabled.
- For large non-float arrays allocated directly on the major heap:
  - if the initializer is a young block, the runtime forces a minor collection first
  - the source comment explicitly says this avoids creating many major-to-minor references
- Array creation/conversion paths often process pending actions before returning so memprof callbacks and runtime work are not starved.

## Array conversion and blit behavior

- `caml_array_of_uniform_array` converts an array literal of boxed floats into a flat float array when possible.
- If the array is empty or the first element is not a boxed float, it returns the original value unchanged.
- Value-array blits use overlap-safe copying and, on multicore paths, fall back to word-by-word release stores instead of plain `memmove`.
- Float-array blit uses `memmove`; the source carries an explicit TODO note about memory-model consistency.

## Compatibility aliases

- Older array-creation names are still present:
  - `caml_make_vect`
  - `caml_make_float_vect`
  - `caml_make_array`

## zort takeaways

- A zort rewrite should keep locale-independent float parsing/formatting. That is an observable language/runtime guarantee, not an incidental libc detail.
- Flat-float-array behavior and the young-to-major avoidance path in `Array.make` are the main runtime-level array semantics worth preserving if zort wants native-code compatibility.
- String/bytes primitives are simple, but their exact bounds, endianness, and C-safety rules are part of the observable runtime contract.

## zort language surface notes

- zort now exposes typed boxed-number accessors in the semantic surface:
  - `allocI32` / `allocI64`
  - `allocF64`
  - `unboxI64`
  - `unboxF64`
- Float parsing/formatting is intentionally locale-stable in the semantic surface:
  - parsing strips `_` separators before delegating to Zig parsing,
  - invalid literals require full-string rejection,
  - formatting special-cases `nan`, `inf`, and `-inf`,
  - ordinary formatting uses Zig's locale-independent formatter.
- Current intentional divergence:
  - zort does not yet claim OCaml-identical coverage for every float literal form,
  - especially around OCaml's dedicated hex-float parser edge cases.
