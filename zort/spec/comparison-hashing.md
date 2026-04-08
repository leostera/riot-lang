# Structural Comparison and Generic Hashing

## Source anchors

- `vendor/ocaml/runtime/compare.c`
- `vendor/ocaml/runtime/hash.c`
- `vendor/ocaml/runtime/caml/custom.h`

## Structural comparison

- OCaml comparison is iterative in C, using an explicit compare stack instead of recursive C calls.
- The compare stack grows dynamically and raises `Out_of_memory` on resize failure / overflow.
- Pending runtime actions are polled periodically during long comparisons.

## Comparison rules

- Immediate integers compare by numeric value.
- Longs sort before blocks.
- Strings compare lexicographically by bytes, then by length.
- Doubles compare numerically, with special handling:
  - partial compare can return unordered for NaNs
  - total compare treats all NaNs as equal and orders NaN below non-NaN floats
- Float arrays compare by length then element-wise with the same NaN policy.
- Objects compare by object ID.
- `Forward_tag` is transparently followed.
- `Infix_tag` is treated as `Closure_tag` for heterogeneous tag comparison.

## Invalid comparisons

- `compare` on these raises `Invalid_argument`:
  - `Abstract_tag`
  - `Closure_tag`
  - `Infix_tag`
  - `Cont_tag`
  - custom blocks whose compare function is missing
- Custom blocks of different custom kinds compare by custom identifier if their compare callbacks differ.

## Generic hashing

- Generic hashing is breadth-first and bounded by:
  - `count`: how many meaningful nodes/items contribute
  - `limit`: queue size used during traversal
- Hashing normalizes:
  - `-0.0` to `+0.0`
  - all NaNs to one canonical NaN payload
- Abstract blocks contribute nothing.
- Object blocks hash by object ID.
- Continuations all hash the same because the runtime does not attempt to distinguish them.
- Custom blocks contribute only if they define a hash callback.
- Closures hash:
  - their cleaned header
  - code pointers / closure-info prefix words
  - reachable environment fields up to the traversal budget
- Variant hashes use a stable arithmetic hash over tag names and are explicitly kept 32/64 compatible.

## zort takeaways

- Structural compare/hash are part of the runtime contract, not merely library sugar.
- If zort decides not to support comparison or hashing of functions/continuations/custom handles, it will be following existing OCaml behavior more than deviating from it.
- The bounded traversal and pending-action polling are worth preserving even if the data representation changes.
