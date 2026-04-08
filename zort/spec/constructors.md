# Value Model, Tags, and Constructors

## Source anchors

- `vendor/ocaml/runtime/caml/mlvalues.h`
- `vendor/ocaml/runtime/alloc.c`
- `vendor/ocaml/runtime/obj.c`

## Observable behavior

- A `value` is either:
  - an immediate integer with low bit `1`
  - a heap block pointer with low bit `0`
  - an encoded out-of-heap pointer via `Val_ptr`, which sets the low bit and must not be mistaken for an OCaml integer by higher-level code
- Immediate integers use `Val_long(x) = (x << 1) + 1` and `Long_val(x) = x >> 1`.
- On 64-bit builds, the immediate signed range is `[-2^62, 2^62 - 1]`.
- Heap blocks carry a header containing:
  - `tag` in the low 8 bits
  - `color` bits used by the GC
  - `wosize`
  - optional reserved bits
- `No_scan_tag` is `251`. Tags below it are scanned as fields. Tags at or above it are raw bytes / non-scanned payloads.

## Special tags

- `250` `Forward_tag`: forwarding pointer. The GC may silently shortcut it.
- `249` `Infix_tag`: infix header inside a closure. Behaves specially during GC, hashing, compare, and closure traversal.
- `248` `Object_tag`: OO objects and exception constructors.
- `247` `Closure_tag`: closure block. Field `0` is code, field `1` is closure-info, later fields may be environment.
- `246` `Lazy_tag`
- `245` `Cont_tag`: continuation blocks used by the effects runtime. The active stack is stored as a tagged `Val_ptr(stack)` payload rather than an ordinary scanned field.
- `244` `Forcing_tag`
- `251` `Abstract_tag`: opaque bytes, never scanned for `value`s.
- `252` `String_tag`
- `253` `Double_tag`
- `254` `Double_array_tag`
- `255` `Custom_tag`

## Constructor-level invariants

- Zero-sized blocks are atoms and are shared per tag with `Atom(tag)`.
- `false`, `unit`, empty list, and `None` all use the immediate integer `0`.
- `true` is immediate integer `1`.
- `Some x` is a one-field block with tag `0`.
- Lists use tag `0` cons cells and `0` immediate for `[]`.
- Exception constructors are `Object_tag` blocks. Exception buckets are ordinary tag-`0` blocks whose field `0` is the constructor.
- Closures have extra structure:
  - field `0`: code pointer
  - field `1`: closure info containing arity and environment start
  - infix headers can appear inside closure blocks
- Effect continuations are not abstract at the runtime level:
  - they are `Cont_tag` blocks
  - the runtime consumes them linearly by clearing the active stack slot on use
  - native perform/reperform logic also uses continuation storage to track the tail of the parent-fiber chain
- Lazy values transition across `Lazy_tag`, `Forcing_tag`, and `Forward_tag` using atomic tag updates in `obj.c`.

## `Obj`-level behavior

- `Obj.tag` returns:
  - `1000` for immediates
  - `1002` for unaligned values
  - otherwise the block tag
- `Obj.new_block` is permissive but not unconstrained:
  - `Closure_tag` requires size at least `2` and gets a sane `Closinfo`
  - `String_tag` requires size greater than `0` and zeroes the last byte so length reads are non-negative
  - `Custom_tag` is rejected with `Invalid_argument`
- `Obj.with_tag` and `Obj.dup` preserve payload shape while changing / reusing tags, with special handling for scanned versus raw blocks.

## zort takeaways

- The first irreversible design decision is whether zort keeps OCaml-style tagged immediates.
- If zort wants to interoperate with generated OCaml code or a shim layer, closure-info, forwarding behavior, and lazy tag transitions matter more than cosmetic API names.
- If zort does not intend to preserve `Obj` tricks, that should be stated as a deliberate break, not left ambiguous.

## zort Semantic Core notes

- zort now keeps OCaml tags as a compatibility concept, not as the internal heap object model.
- Internal heap objects are semantic kinds:
  - tuple
  - string
  - boxed_i64
  - boxed_f64
  - custom
- `HeapRef` is the stable heap identity used by `Value.block`.
- Tag translation is derived on demand from the semantic object kind when compatibility-oriented code needs it.
- Internal tests should prefer semantic assertions (`kind`, typed boxed payload access, string buffer access) over raw tag and byte-layout assertions.
