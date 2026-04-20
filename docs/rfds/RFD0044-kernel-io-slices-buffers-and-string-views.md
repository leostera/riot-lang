# RFD0044 - Kernel IO Buffers, Slices, and Buffered Readers

- Feature Name: `kernel_io_buffers_slices_and_buffered_readers`
- Start Date: `2026-04-19`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD proposes the final shape of Riot's low-level I/O model around owned
off-heap buffers, borrowed byte slices, and explicit copy boundaries. The
kernel owns the syscall-facing storage. `std` re-exports that storage and builds
a small, consistent `Reader` / `Writer` / `BufReader` surface on top. The goal
is not to make every public API borrowed by default. The goal is to make the
fast transport path the default substrate, while making ownership changes
explicit and teachable.

- `Kernel.IO` should expose off-heap `IoSlice`, `IoVec`, and `Buffer` as the
  syscall-facing byte model
- `Std.IO.Buffer` should be the default off-heap buffer in `std`, with
  `Std.StringBuilder` reserved for explicit heap text accumulation
- `Std.IO.Reader` and `Std.IO.Writer` should be buffer-first; `Std.IO.BufReader`
  should be the single borrowed-slice layer in the standard library
- `from_*` / `to_*` APIs should remain the explicit copy boundaries between
  off-heap storage and normal OCaml heap values
- this RFD does not propose a generic incremental parser framework, a public
  borrowed HTTP API by default, or a single global `Std.IO` error type

## Motivation
[motivation]: #motivation

Riot had two intertwined problems.

First, the transport substrate was not explicit enough. Heap `string`, heap
`bytes`, and off-heap syscall buffers kept getting treated as if they were the
same thing. They are not. Heap values are convenient, but they are the wrong
thing to retain across blocking native I/O. Off-heap storage is stable at that
boundary, but until now it was not the obvious default currency of `Std.IO`.

Second, the standard-library surface encouraged accidental ownership changes.
When a reader returns strings, or a buffered helper materializes by default,
contributors cannot tell where copies happen without reading the
implementation. That makes performance regressions hard to spot and harder to
review.

Recent HTTP and `Std.IO` benchmark work clarified the actual trade-off.

- parser-only large-body request parsing benefits substantially from staying on
  `IoSlice` until the final ownership boundary
- full-request reader-driven parsing benefits from the off-heap substrate once
  ingestion uses caller-owned buffers instead of bouncing through heap strings
- small and header-heavy workloads still favor a direct string parser, which
  means the public parser surface should stay ergonomic even if the transport
  substrate becomes slice-first

The standard-library benchmarks tell the same story in a smaller setting.
Borrowed `BufReader` slices are useful, but they are only clearly better when
they avoid repeated tiny materializations. For larger lines, borrowed slices and
explicit `to_string` are within a few percent of the owned helper. That means
the substrate matters, but the public API should stay honest about where
ownership changes actually pay off.

This is the important lesson. Riot does not need every public API to become
borrowed. Riot does need one clear I/O substrate:

- kernel owns stable off-heap byte storage
- `std` re-exports it
- raw readers and writers fill caller-owned buffers
- buffered readers borrow slices from their internal buffer
- string materialization is explicit

If Riot does nothing, we should expect more drift back toward heap buffers in
hot paths, more one-off fixes around blocking writes, and more confusion about
where ownership changes actually happen.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The intended mental model is the same split that Go and Rust use:

- raw readers fill caller-owned buffers
- buffered readers may hand out borrowed slices from an internal buffer

For Riot that means:

- `Std.IO.Buffer` owns contiguous off-heap bytes
- `Std.IO.IoSlice` is a borrowed byte view
- `Std.IO.IoVec` is a vectored collection of slices
- `Std.IO.Reader` fills caller-owned `Buffer` or `IoVec` values
- `Std.IO.BufReader` borrows `IoSlice`s from its internal buffer
- `Std.StringBuilder` is the explicit heap text builder

So the default transport path should feel like:

```ocaml
let buf = Std.IO.Buffer.create ~size:4096 in
let* _ = Std.IO.Reader.read reader ~into:buf in

let slice = Std.IO.Buffer.readable buf in
if Std.IO.IoSlice.starts_with slice ~prefix:"GET " then
  ...
```

And the default buffered path should feel like:

```ocaml
let br = Std.IO.BufReader.from_reader reader in
let* line = Std.IO.BufReader.read_line br in
let owned = Std.IO.IoSlice.to_string line in
...
```

The important part is what does not happen by default:

- raw `Reader.read` does not allocate and return a hidden borrowed slice
- `BufReader.read_line` does not materialize a string unless the caller asks for it
- `Std.IO.Buffer` is not a heap string builder

The ownership boundary is explicit:

```ocaml
let path = Std.IO.IoSlice.to_string borrowed_path
```

That says exactly where transport bytes become application-owned text.

### What contributors should assume

- hot transport paths should default to `Buffer`, `IoSlice`, and `IoVec`
- `from_*` and `to_*` APIs are copy boundaries
- range-sensitive operations return `result` by default
- `_unchecked` variants exist for validated inner loops, not as the normal API
- public protocol APIs may still expose `string` where that remains the most
  useful application-facing type

### What contributors should not assume

- that heap `bytes` is the right default buffer type for blocking native I/O
- that moving one parser onto borrowed slices automatically improves performance
- that raw `Reader.read` should return a borrowed view
- that zero-copy means "Riot never copies"; it means copies happen at explicit
  boundaries instead of as the default transport model

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Core kernel types

### 1.1 `Kernel.IO.IoSlice`

`IoSlice` is the basic off-heap byte view. It should:

- support zero-copy sub-slicing
- keep text and delimiter helpers directly on the slice
- use checked-by-default range-sensitive operations
- keep `_unchecked` variants for hot loops that have already validated bounds
- treat `from_*` / `to_*` as explicit copy boundaries

### 1.2 `Kernel.IO.IoVec`

`IoVec` is the narrow vectored-I/O type for `readv` / `writev`. It should stay:

- syscall-facing
- off-heap
- cheap to flatten or sub-slice when needed

It is not a general string builder and not a parser abstraction.

### 1.3 `Kernel.IO.Buffer`

`Kernel.IO.Buffer` is the owned contiguous off-heap buffer. It should:

- expose readable and writable regions as `IoSlice`
- support compaction and growth
- be the kernel-side storage primitive that `std` re-exports as `Std.IO.Buffer`

## 2. `std` layering

### 2.1 `Std.IO.Buffer`

`Std.IO.Buffer` is the standard-library default off-heap buffer. It is the
normal thing `Reader` and `Writer` operate on.

`Std.IO.IoBuffer` may remain as the exact kernel-shaped API, but code above
`std` should normally use `Std.IO.Buffer`.

### 2.2 `Std.StringBuilder`

`Std.StringBuilder` is the explicit heap text builder. It exists for:

- `read_to_string`
- text rendering
- string accumulation that is intentionally heap-owned

It should not be the default transport buffer in `Std.IO`.

### 2.3 `Std.IO.Reader`

`Reader` is buffer-first. Its public contract is:

- `read`
- `read_vectored`
- `is_read_vectored`
- `read_to_end`
- `read_to_string`
- `read_exact`
- `bytes`
- `chain`
- `take`

The important rule is that the raw reader does not return hidden borrowed data.
The caller owns the destination buffer.

### 2.4 `Std.IO.BufReader`

`BufReader` is the only borrowed-slice layer in `std`. Its public contract is:

- `from_reader`
- `read`
- `read_byte`
- `size`
- `reset`
- `fill`
- `peek`
- `consume`
- `read_rune`
- `read_slice`
- `read_line`
- `read_string`
- `to_reader`

`peek` should be exact-or-error. `read_slice` should follow Go's shape:

- return a borrowed slice including the delimiter when found
- return `Buffer_full` if the internal buffer fills before the delimiter appears
- return the remaining tail at EOF if any bytes were buffered

### 2.5 `Std.IO.Writer`

`Writer` is the symmetric buffer-first write surface:

- `write`
- `write_vectored`
- `write_all`
- `write_all_vectored`
- `flush`

`Writer` accepts `Buffer` and `IoVec`, not heap strings as its core currency.

## 3. Error and fallibility model

Range-sensitive operations should return `result`.

That includes:

- `IoSlice.sub`
- `IoSlice.shift`
- `IoSlice.split_at`
- `IoSlice.get`
- `IoSlice.set`
- `IoSlice.blit`
- `IoVec.sub`
- `Buffer.ensure_free`
- `Buffer.commit`
- `Buffer.consume`

Hot loops that have already validated bounds may use `_unchecked` helpers.

`std` should keep upstream error types where they matter. `Reader.t` and
`Writer.t` remain parameterized by the source or sink error instead of forcing
all of `Std.IO` through one broad public sum type. `BufReader` may add its own
small error layer for buffered-reader-specific failures such as `Buffer_full`,
but it should not erase meaningful upstream errors.

## 4. Borrowing and ownership rules

Borrowed slices are only useful if their lifetime rules are explicit.

This RFD standardizes the following rule:

- a slice borrowed from a caller-owned `Std.IO.Buffer` is valid until that
  buffer is mutated or dropped
- a slice borrowed from `Std.IO.BufReader` is valid until the next operation
  that may refill, consume, or reset the internal buffer

That is the same contract shape as:

- Go `bufio.Reader.ReadSlice`
- Rust `BufRead::fill_buf` plus `consume`

If a caller wants to keep a value longer, it must either:

- keep the owning buffer alive and stable
- or copy with `IoSlice.to_string` / `IoSlice.to_bytes`

## 5. Syscall boundary rules

At the blocking syscall boundary:

- payload pointers passed to blocking native calls must come from off-heap
  storage
- heap `string` / `bytes` payload pointers must not be retained across
  `caml_enter_blocking_section()`
- vectored operations should use `IoVec` / `IoSlice`, not OCaml heap slices

That means the correct low-level path is:

1. allocate or reuse off-heap buffers
2. expose `IoSlice` / `IoVec` views
3. call the kernel
4. materialize heap values only after the I/O boundary when ownership truly changes

## 6. Migration strategy

The intended rollout is:

1. stabilize `Kernel.IO.IoSlice`, `Kernel.IO.IoVec`, and `Kernel.IO.Buffer`
2. keep `Std.IO.Buffer` as the default off-heap buffer surface
3. keep `Reader` and `Writer` buffer-first
4. make `BufReader` the single borrowed-slice layer in `std`
5. move protocol parsers onto `IoSlice` only where benchmarks justify it
6. keep public string-owning APIs where they remain the ergonomic default

## Drawbacks
[drawbacks]: #drawbacks

The main cost of this design is cognitive.

Contributors now need to reason about:

- the difference between owned buffers and borrowed slices
- explicit ownership boundaries
- borrowed slice lifetime rules
- checked versus `_unchecked` operations

There is also real implementation cost:

- kernel and std migration work
- benchmark maintenance
- downstream parser rewrites where the payoff is not guaranteed

Not every parser should become borrowed by default. The HTTP work already
showed that large-body transport paths benefit, while small and header-heavy
workloads may still favor a direct string parser.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why not keep `StringView`?

`StringView` did not carry enough distinct value once `IoSlice` grew the text
helpers we actually needed.

- it duplicated operations that already belonged naturally on `IoSlice`
- it added a third concept where two were enough
- it did not solve the real lifetime problem, which is about who owns the backing buffer

The final design keeps text helpers on `IoSlice` and uses `to_string` as the
explicit ownership boundary.

### Why not make raw `Reader.read` return a slice?

Because that makes ownership less clear.

If raw `read` returns a slice, then either:

- the reader allocates a fresh buffer every call
- or it returns a borrowed slice from hidden internal storage

The first hides allocation in the hot path. The second hides borrowing in the
raw reader contract. The cleaner split is:

- `Reader`: caller-buffered
- `BufReader`: borrowed-slice layer

### Why not collapse all `Std.IO` errors into one type?

Rust can do that because its standard library owns a much broader cross-cutting
I/O error model. Riot's `std` surface already carries meaningful domain errors
from TLS, gzip, command execution, and other sources.

For Riot the better trade-off is:

- keep `Reader.t` and `Writer.t` parameterized by the underlying error
- let `BufReader` add only the small extra errors it truly owns

### Why not make every public parser borrowed?

Because the benchmark evidence does not justify that complexity for every API.

The right outcome is:

- slice-first transport and buffering
- borrowed parser paths where they measurably help
- public string-owning APIs where that remains easier to use and not obviously slower

## Prior art
[prior-art]: #prior-art

Riot is converging on the same split used by other high-performance systems.

### Go

- `io.Reader.Read(p []byte)` fills caller-owned buffers
- `bufio.Reader.ReadSlice(delim)` returns borrowed slices from an internal buffer
- converting `[]byte` to `string` is an explicit ownership change

### Rust

- `Read::read(&mut [u8])`
- `Read::read_vectored(...)`
- `BufRead::fill_buf()`
- `BufRead::consume(n)`

Rust encodes the buffered lifetime rule in the type system. Riot documents the
same rule in API contracts.

### OCaml ecosystem

- `bigstringaf` provides stable off-heap storage
- Eio's bigstring / `Cstruct` flow model keeps transport bytes off the OCaml heap
- higher-level HTTP stacks commonly leave bodies lazy or streaming instead of
  materializing them during head parsing

## Unresolved questions
[unresolved-questions]: #unresolved-questions

The remaining open questions are:

- should Riot add a `BufWriter` symmetric to `BufReader`?
- should `bytes()` remain on `Reader`, or stay a convenience adapter outside the core hot path?
- should Riot add a generic incremental parser substrate later, or keep parser state machines package-specific?
- should long-lived retained borrowed views eventually gain a first-class retained-buffer type rather than relying on "keep the owning buffer alive"?

## Future possibilities
[future-possibilities]: #future-possibilities

Future follow-up work may include:

- `BufWriter` and other buffered write helpers
- incremental HTTP head parsers and other stateful streaming parsers on top of `BufReader`
- retained-view or chunk-retention abstractions for long-lived borrowed values
- more parser migrations where benchmarks justify the extra complexity
- clearer public docs that compare Riot's model directly to Go and Rust
