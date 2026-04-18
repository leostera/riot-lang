# RFD0044 - Kernel IO Slices, Buffers, and String Views

- Feature Name: `kernel_io_slices_buffers_and_string_views`
- Start Date: `2026-04-19`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD proposes a redesign of Riot's low-level I/O substrate around off-heap
byte storage, cheap slice views, and explicit copy boundaries. It is a kernel
and `std` contract change, not just an internal optimization pass. The goal is
to make zero-copy or low-copy I/O the default model for hot paths, while making
heap `string` / `bytes` conversions explicit and teachable.

- `Kernel.IO` should default to off-heap `IoSlice`, `Iovec`, `IoBuffer`, and
  `StringView` types instead of making heap `string` / `bytes` the normal I/O
  currency
- fallible range-sensitive operations should return `result` values by default,
  with `_unchecked` variants reserved for validated hot loops
- `from_*` / `to_*` APIs should be the explicit copy boundaries between
  off-heap I/O storage and normal OCaml heap values
- `std` should build buffered readers and writers on top of these types instead
  of teaching protocol code to parse transport bytes as heap strings first
- this RFD does not propose capability-typed slices, a public protocol API
  migration, or a full parser rewrite rollout yet

## Motivation
[motivation]: #motivation

Riot is currently paying for an I/O model that is both hard to reason about and
not yet fast enough to justify its complexity.

The immediate trigger for this RFD is recent work on `Kernel.IO`, `Std.IO`, and
the HTTP/1 parser. That work exposed three structural problems.

First, the current model makes it too easy to blur together three very
different kinds of bytes:

- heap `string`
- heap `bytes`
- off-heap syscall-facing buffers

These are not interchangeable. Heap `string` and `bytes` are convenient OCaml
values, but they are not stable syscall buffers across
`caml_enter_blocking_section()`. Off-heap buffers are stable, but they are not
the default currency of most Riot APIs today. That mismatch has already shown
up as a real correctness hazard: blocking native I/O paths cannot safely retain
raw pointers into heap-managed strings or bytes once the runtime lock is
released. Riot has already had to patch individual print paths to copy into
C-owned memory before blocking writes. That is a symptom of the wrong default
model.

Second, the current model forces contributors to guess where copying is
happening.

Today Riot has several overlapping patterns:

- some APIs accept or return `string`
- some accept or return `bytes`
- some now use `IoSlice`, `Iovec`, or `StringView`
- some paths convert eagerly at the boundary
- some keep parsing on heap strings

That means the performance model is unclear. A contributor looking at an API
cannot easily answer:

- is this path zero-copy?
- is this path metadata-only slicing?
- is this copying heap bytes into off-heap storage?
- is this converting back to heap strings again?

That uncertainty is not just cosmetic. It makes reviews slower, optimizations
harder, and regressions easier to hide.

Third, our recent HTTP parser experiment showed that moving one parser onto an
off-heap view layer is not enough if the surrounding transport substrate is
still expensive.

The benchmark results from that experiment were decisive:

- parser-only, `1 MiB` request:
  - old string parser: `3.67 ms`
  - new public `parse`: `24.41 ms`
  - new direct `parse_string_view`: `11.84 ms`
- reader-fed, `1 MiB` request:
  - old path: `103.78 ms`
  - new public `parse`: `199.88 ms`
  - new direct `parse_string_view`: `290.60 ms`

Those numbers do not justify selling the parser migration as a performance win.
The real lesson is different: Riot tried to optimize a protocol parser before
the underlying buffer and copy model had settled. We added off-heap types, but
we did not yet make off-heap buffers the dominant transport currency.

This cost is structural rather than incidental.

- as long as heap strings and bytes remain the easy default, contributors will
  keep reaching for them in hot I/O code
- as long as copy boundaries are implicit, the cost model will stay hard to
  audit
- as long as protocol code starts from heap strings, Riot will keep paying to
  move data into and out of transport buffers instead of parsing closer to the
  wire
- as long as syscall-facing code is allowed to drift back toward heap buffers,
  correctness hazards around blocking sections will keep resurfacing

The opportunity is also structural.

If Riot adopts one clear I/O model, contributors can build:

- safer `readv` / `writev` paths over stable off-heap slices
- buffered readers and writers that expose zero-copy views by default
- parsers that operate on borrowed byte or string views and only materialize
  heap strings when a public API genuinely needs them
- transport adapters where copying happens at explicit, named conversion points
  instead of being rediscovered ad hoc

That makes the likely benefits of this redesign:

- correctness at the blocking syscall boundary
- a teachable performance model
- lower transport overhead once the core buffer types are mature
- clearer separation between transport bytes and application-facing heap values

If Riot does nothing, we should expect to keep paying in several ways:

- more one-off fixes for unsafe heap-pointer use near blocking syscalls
- more local reinventions of buffer and slice semantics
- more protocol experiments that add complexity without delivering throughput
  wins
- ongoing confusion about whether `bytes` is "good enough" as a default I/O
  buffer type

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Suppose a contributor wants to add a fast line-oriented protocol parser on top
of a socket.

Today the easiest path in Riot is still to think in heap values first:

1. read some bytes
2. turn them into a `string` or `bytes`
3. split or slice that heap value
4. build more heap strings as the parser progresses

That is easy to write, but it mixes together three concerns:

- transport buffering
- parsing
- materializing application-facing values

With this proposal, contributors should think in a different order:

1. transport fills an `IoBuffer`
2. code gets an `IoSlice` or `StringView` over the readable bytes
3. parser scans and slices those views
4. buffer consumption advances without copying
5. heap `string` / `bytes` values are only created when the API actually wants
   to keep data as normal OCaml values

The new mental model is:

- `IoBuffer` owns contiguous off-heap bytes
- `IoSlice` is a cheap byte view over some region of off-heap storage
- `Iovec` is a collection of slices for vectored I/O
- `StringView` is a read-only string-like view over transport bytes

So the default transport path should feel like:

```ocaml
let* buf = Std.IO.IoBuffer.create () in
let* n = Std.IO.Reader.read reader ~dst:(Std.IO.IoBuffer.writable buf) in
let* () = Std.IO.IoBuffer.commit buf n in

let view =
  Std.IO.IoBuffer.readable buf
  |> Std.IO.StringView.from_slice
in

match Std.IO.StringView.index_string view "\r\n\r\n" with
| None -> (* need more bytes *)
| Some boundary ->
  let* head = Std.IO.StringView.sub view ~off:0 ~len:boundary in
  (* parse request head without copying *)
  let* () = Std.IO.IoBuffer.consume buf (boundary + 4) in
  ...
```

When a caller explicitly needs a normal OCaml string, the boundary is obvious:

```ocaml
let request_target = Std.IO.StringView.to_string target_view
```

That explicit `to_string` is important. It says:

- this is where heap allocation happens
- this is where transport bytes become application-owned text

The same change matters on the write side.

Today it is easy to build up output as repeated heap strings and let the system
sort it out later. With this proposal, the intended flow is:

1. write into an `IoBuffer`
2. expose one or more `IoSlice`s
3. send them with `write` or `writev`

Heap strings are still supported, but they become explicit adapters:

```ocaml
let* () = Std.IO.IoBuffer.append_string buf "GET / HTTP/1.1\r\n" in
let* () = Std.IO.IoBuffer.append_string buf "Host: example.com\r\n\r\n" in
let* () = Std.IO.Writer.write writer ~src:(Std.IO.IoBuffer.readable buf) in
```

That example still copies from heap strings into the off-heap buffer. The point
is not that copies disappear completely. The point is that the system becomes
honest about where they happen.

### What contributors should assume

- hot transport paths should default to `IoBuffer`, `IoSlice`, `Iovec`, and
  `StringView`
- `from_*` and `to_*` APIs are copy boundaries
- range-sensitive operations return `result` by default
- `_unchecked` variants exist for validated inner loops, not as the normal API
- public protocol APIs may still expose `string` where that remains the most
  useful application-facing type

### What contributors should not assume

- that heap `bytes` is the right default buffer type for blocking native I/O
- that moving one parser onto a view type automatically improves performance
- that vectored syscalls matter more than contiguous off-heap buffering
- that zero-copy means "Riot never copies"; it means copies happen at explicit
  boundaries instead of as the default transport model

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Core types

This proposal introduces or stabilizes four core low-level types.

### 1.1 `Kernel.IO.IoSlice`

`IoSlice` is the basic byte-view type for low-level I/O.

It should:

- be backed by off-heap storage
- support zero-copy sub-slicing
- expose checked-by-default range operations that return `result`
- provide `_unchecked` operations for validated hot paths
- expose explicit copy adapters such as `from_string`, `from_bytes`,
  `to_string`, and `to_bytes`

It should not:

- hide copying behind ordinary slicing operations
- present heap `string` / `bytes` as though they were syscall-safe buffers

The intended API shape is roughly:

```ocaml
module Kernel.IO.IoSlice : sig
  type t
  type error = Kernel.IO.Error.t

  val empty : t
  val create : size:int -> (t, error) result
  val length : t -> int

  val sub : t -> off:int -> len:int -> (t, error) result
  val shift : t -> int -> (t, error) result
  val split_at : t -> int -> ((t * t), error) result

  val get : t -> at:int -> (char, error) result
  val get_unchecked : t -> at:int -> char

  val set : t -> at:int -> char -> (unit, error) result
  val set_unchecked : t -> at:int -> char -> unit

  val blit :
    src:t -> src_off:int ->
    dst:t -> dst_off:int ->
    len:int ->
    (unit, error) result

  val blit_unchecked :
    src:t -> src_off:int ->
    dst:t -> dst_off:int ->
    len:int ->
    unit

  val blit_from_string :
    string -> src_off:int -> t -> dst_off:int -> len:int -> (unit, error) result

  val blit_from_bytes :
    bytes -> src_off:int -> t -> dst_off:int -> len:int -> (unit, error) result

  val blit_to_bytes :
    t -> src_off:int -> bytes -> dst_off:int -> len:int -> (unit, error) result

  val from_string : ?off:int -> ?len:int -> string -> (t, error) result
  val from_bytes : ?off:int -> ?len:int -> bytes -> (t, error) result

  val to_string : t -> string
  val to_bytes : t -> bytes
end
```

### 1.2 `Kernel.IO.Iovec`

`Iovec` is the syscall-facing vectored I/O shape.

It should be little more than:

- `IoSlice.t array`
- length accounting
- shifting after partial writes

This proposal intentionally keeps `Iovec` small. The important work should
happen in `IoSlice` and `IoBuffer`, not in a complicated vector abstraction.

### 1.3 `Kernel.IO.IoBuffer`

`IoBuffer` is the main mutable contiguous off-heap buffer type.

It should:

- own contiguous off-heap storage
- expose readable and writable slices
- support `ensure_free`, `commit`, `consume`, and `compact`
- make transport fill and drain operations explicit
- support explicit append helpers for `string`, `bytes`, and `IoSlice`

It should be the normal substrate for:

- buffered readers
- buffered writers
- protocol framing code
- socket and file ingestion

### 1.4 `Kernel.IO.StringView`

`StringView` is a read-only string-like view over an `IoSlice`.

It exists because protocol parsers often need:

- `length`
- `get`
- `sub`
- `shift`
- `starts_with`
- `index_char`
- `index_string`

but do not actually need to materialize heap strings for those operations.

`StringView` should stay small and read-only. It is a parsing aid, not a second
mutable buffer type.

## 2. Error and fallibility model

Range-sensitive operations should return `result` values by default.

That includes operations such as:

- `sub`
- `shift`
- `split_at`
- `get`
- `set`
- `blit`
- `consume`
- `commit`
- `ensure_free`

This RFD prefers checked-by-default APIs because the kernel and `std` surfaces
are shared infrastructure. Unsafe indexing and silent truncation are the wrong
defaults there.

For hot loops, explicit `_unchecked` variants are allowed when a caller has
already validated its bounds.

`to_string` and `to_bytes` remain total APIs. They are explicit copy boundaries
rather than range-sensitive operations.

## 3. Borrowing and view rules

This proposal assumes the simplest useful borrowing model first.

- `IoSlice.sub` is zero-copy and shares backing storage
- `StringView.sub` is zero-copy and shares backing storage
- `IoBuffer.readable` and `IoBuffer.writable` return borrowed views into the
  buffer's current storage

Those borrowed views are only intended to stay valid until the next mutating
operation that may resize, compact, or otherwise change the underlying buffer.

This is deliberately simple. It matches what Riot needs for the initial
transport and parser work without forcing a segmented or reference-counted
buffer design immediately.

If Riot later needs long-lived zero-copy views that survive buffer mutation,
that should be a follow-up design, not part of this first redesign.

## 4. Syscall boundary rules

The kernel boundary should be off-heap-first.

That means:

- `read` writes into an `IoSlice`
- `write` reads from an `IoSlice`
- `readv` and `writev` operate on `Iovec`

The rule at the native boundary is:

- payload pointers that cross `caml_enter_blocking_section()` must be stable
- heap `string` and `bytes` are not acceptable payload buffers there
- off-heap `IoSlice` storage is acceptable

This is one of the main reasons to make `IoSlice` the default currency.

## 5. `std` layering

Above kernel, `std` should re-expose these types as the main I/O substrate.

`Std.IO` should:

- expose `IoSlice`, `Iovec`, `IoBuffer`, and `StringView`
- build reader and writer helpers on those types
- provide buffered readers and writers that operate on them
- keep `stdin`, `stdout`, and `stderr` ergonomic while still using the same
  underlying model

The key point is that `std` should not reintroduce heap `string` / `bytes` as
the default transport representation just because those values are ergonomic.

## 6. Migration strategy

This proposal is intentionally staged.

### Stage 1

Stabilize the substrate:

- `IoSlice`
- `Iovec`
- `IoBuffer`
- `StringView`
- bulk-native `blit*` operations

### Stage 2

Move `Std.IO` buffered reader and writer code onto that substrate.

### Stage 3

Re-evaluate hot protocol paths after the buffer model is cheap and explicit.

This RFD does not assume that every existing parser should immediately migrate.
The HTTP/1 experiment showed that parser-first migration is the wrong order.

## Drawbacks
[drawbacks]: #drawbacks

This redesign has real costs.

- It introduces several new foundational types at once.
- It makes more of Riot's I/O story explicit, which increases the amount of API
  surface contributors need to learn.
- It draws a harder line between transport bytes and heap strings, which can
  make some application-facing code feel less direct.
- It will likely force some churn in `Kernel.IO`, `Std.IO`, and packages that
  currently move freely between `string`, `bytes`, and transport buffers.
- If the implementation is careless, Riot could end up paying the complexity
  cost without getting the transport wins.

There is also a design risk: Riot could overfit to one low-level I/O model and
make higher-level protocol code less pleasant than it needs to be.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Do nothing

Riot could keep the current mixed model and continue fixing unsafe or slow paths
locally.

That would be cheaper short-term, but it would preserve the structural
problems:

- unclear copy boundaries
- continued pressure to use heap `bytes` as transport buffers
- repeated local fixes around blocking syscalls
- parser and transport work proceeding without one clear low-level contract

### Keep heap `bytes` as the main mutable buffer type

This is attractive because `bytes` is familiar and ergonomic.

The problem is that heap `bytes` is not the same thing as a stable off-heap I/O
buffer. Across blocking native calls, that distinction matters. Using heap
`bytes` as the default low-level I/O substrate invites correctness hazards and
confuses the transport model.

### Push parser migrations first

The recent HTTP experiment already tested this path indirectly.

That experiment showed that moving one parser onto off-heap views before the
underlying buffer and copy substrate is mature is the wrong order. Parser
throughput did not improve, and in several cases it regressed badly.

### Adopt an existing slice type wholesale

Riot could choose to adopt a `Cstruct`-style slice representation or another
third-party I/O substrate directly instead of designing its own.

That remains a plausible future direction, but this proposal prefers a
Riot-owned kernel contract because:

- Riot is already redesigning `Kernel.IO`
- Riot wants `std` to sit almost directly on top of kernel-owned contracts
- Riot has specific rules around result-valued APIs, naming, and blocking-call
  safety that may not align exactly with existing libraries

### Add capability-typed slices now

Capability-typed slices may still be worthwhile later, but this RFD leaves them
out intentionally.

They add API and migration churn before Riot has settled the actual slice and
buffer model. The current priority is to stabilize the transport currency and
copy semantics first.

## Prior art
[prior-art]: #prior-art

### Eio

Eio is the strongest prior art for the low-level direction proposed here.

It uses:

- bigarray-backed contiguous buffers
- `Cstruct.t` as the slice/view type
- real `readv` / `writev` in its POSIX backend
- buffered readers and writers built on that substrate

The important lesson is not just that Eio uses vectored syscalls. It is that
Eio makes off-heap slices the default low-level transport currency and copies
heap strings or bytes only when crossing that boundary explicitly.

### bigstringaf

`bigstringaf` is useful prior art for one narrow piece of the design:

- one off-heap contiguous byte-array type
- cheap subviews
- fast bulk `memcpy` / `memcmp` / `memchr` helpers

It does not define an `iovec` model itself. That is a useful reminder that
Riot's core need is a good off-heap byte substrate first, not a complicated
vector abstraction first.

### Piaf

Piaf uses `Bigstringaf.t` plus logical iovecs, but it does not itself appear
to rely on syscall-level `readv` / `writev` in the same way Eio does. Its model
reinforces another useful lesson:

- stable off-heap buffers matter more than exposing a vector abstraction
  everywhere

### ocaml-tls

`ocaml-tls` is useful as a counterexample.

Its current core engine has moved toward `string` / `bytes` rather than keeping
an off-heap slice type in the middle. That makes sense for a pure protocol
engine, but it also shows a different design choice:

- keep the protocol core simple and pure
- convert at the transport boundary

Riot should borrow from that lesson selectively. It is a good reason not to
force every public protocol API to become an off-heap view API. It is not a
good reason to keep heap `string` / `bytes` as the default low-level transport
currency.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should `IoSlice` be implemented as a thin wrapper over a bigarray-like slice
  record, or should it eventually converge more directly on a `Cstruct`-like
  layout?
- Should `IoBuffer` remain strictly contiguous in its first version, or is
  there already enough evidence to justify segmented storage?
- Which `Std.IO` text operations should return `StringView` first, and which
  should continue returning `string` for ergonomics?
- Where should Riot draw the line between transport-oriented APIs and
  application-oriented helpers in `std`?
- Once the core substrate is stable, which protocol package should be the next
  serious benchmark target after HTTP/1?

## Future possibilities
[future-possibilities]: #future-possibilities

If this redesign lands and proves out, there are several natural follow-ons.

- Add capability-typed slices once the underlying slice and buffer contracts
  have stabilized enough that the extra type churn is worth it.
- Revisit protocol parsing packages such as HTTP, WebSocket, and framing-heavy
  tooling on top of the new buffer model once the transport substrate is
  genuinely cheap.
- Add borrowed-buffer read hooks in `Std.IO` so copy-free transport-to-transport
  paths can avoid even temporary `IoBuffer` shuffling where the source already
  owns readable slices.
- Explore whether Riot should keep converging toward an Eio- or `Cstruct`-like
  surface, or whether the long-term value of a Riot-owned shape justifies
  continuing to diverge.
- Expand the same model to runtime-owned logging, TLS adapters, and other
  boundaries where Riot currently converts eagerly into heap strings or bytes.
