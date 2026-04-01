# RFD0029 - Std Archive and Compression Support

- Feature Name: `std_archive_and_compression`
- Start Date: `2026-04-01`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes adding first-class archive and compression support to `std`
with the following public module split:

- `Std.Archive.Tar`
- `Std.Archive.Zip`
- `Std.Compress.Gzip`
- `Std.Compress.Zlib`
- `Std.Compress.Flate`

The implementation strategy is explicitly native, not pure OCaml.
`kernel` will provide narrow mechanical bindings to ubiquitous C libraries,
while `std` will expose the stable Riot-facing API.

One important runtime constraint applies to the whole design:

- the `kernel` surface must be incremental and stateful
- it must not expose monolithic "compress this whole file" or "extract this
  whole archive" C calls that occupy a Riot scheduler for the duration of the
  operation

High-level file- and path-oriented convenience APIs may still exist in `std`,
but they must be implemented as chunked loops over Riot I/O primitives on top
of resumable native engines.

The external native dependency plan is:

- `zlib` as a `kernel` dependency for `flate`, `zlib`, `gzip`, and checksum
  support required by gzip/zip

Archive handling remains part of the native `kernel` implementation, but this
RFD does not standardize any additional external archive library dependency.

This RFD intentionally separates archive formats from compression codecs:

- `tar` and `zip` are archives
- `gzip`, `zlib`, and `flate` are compression/container codecs

The goal is to make common archive operations available without shelling out to
external programs such as `tar`, `gzip`, or `unzip`, and without introducing
extra OCaml package dependencies.

## Motivation
[motivation]: #motivation

Riot already needs archive functionality in practice.
Today, `packages/pkgs-ml/src/registry.ml` shells out to `tar` in order to:

- list archive contents
- find the synthetic archive root directory
- extract a subtree into the package source cache

That works, but it has several problems:

- it depends on external tools being present in the runtime environment
- error handling is stringly and shell-command oriented
- it is harder to reason about in cross-platform and sandboxed environments
- package installation logic is coupled to command-line archive tools instead of
  Riot's own standard library

The same need is likely to show up in more places:

- package publication wants tarball creation
- package installation wants tar or tar+gzip extraction
- future tooling may need zip bundle support
- generic Riot applications should not need to add their own shell wrappers or
  third-party OCaml libraries for basic archive tasks

There is also a naming and modeling concern.
Archive formats and compression codecs are related but not interchangeable.
If Riot exposes them, the module tree should reflect that clearly instead of
blurring the boundary.

Finally, Riot already has a low-level C integration boundary in `kernel`.
It is the right place to depend on boring, ubiquitous native libraries in the
same style that Riot already uses for TLS and UUID support.

There is also a runtime concern beyond naming and library choice.
Riot is a cooperative runtime. A long-running native "extract this archive" or
"compress this file" call would block the owning scheduler/domain for the whole
operation. That makes a monolithic C boundary the wrong fit even if the backing
native library supports it.

The right design is:

- native codec/archive state machines in `kernel`
- chunked reads and writes through Riot file and I/O abstractions in `std`
- convenience helpers in `std` that loop over those engines without turning the
  native boundary into one giant blocking operation

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

After this change, contributors should be able to treat archive and compression
support as normal `Std` functionality.

### Namespacing

The public surface is:

- `Std.Archive.Tar`
- `Std.Archive.Zip`
- `Std.Compress.Gzip`
- `Std.Compress.Zlib`
- `Std.Compress.Flate`

That means:

- if you want to inspect or extract a tarball, you reach for `Std.Archive.Tar`
- if you want to inspect or extract a zip archive, you reach for `Std.Archive.Zip`
- if you want to compress or decompress gzip data, you reach for `Std.Compress.Gzip`

This mirrors the conceptual split used by other standard libraries such as Go:

- `archive/tar`
- `archive/zip`
- `compress/gzip`
- `compress/zlib`
- `compress/flate`

### User model

The first cut should still feel basic and file-oriented from `std`.
Riot code should be able to:

- list entries in a tar or zip archive
- extract an archive into a directory
- create a tar or zip archive from files
- compress or decompress a file using gzip
- compress or decompress in-memory strings/bytes using gzip/zlib/flate

Example shape:

```ocaml
open Std

let () =
  Compress.Gzip.decompress_file
    ~src:(Path.v "package.tar.gz")
    ~dst:(Path.v "package.tar")
  |> Result.expect ~msg:"Failed to gunzip archive"

let entries =
  Archive.Tar.entries (Path.v "package.tar")
  |> Result.expect ~msg:"Failed to inspect tarball"

let () =
  Archive.Tar.extract
    ~archive:(Path.v "package.tar")
    ~into:(Path.v "_tmp/package")
  |> Result.expect ~msg:"Failed to extract tar archive"
```

Or for zip:

```ocaml
let entries =
  Archive.Zip.entries (Path.v "bundle.zip")
  |> Result.expect ~msg:"Failed to inspect zip archive"

let () =
  Archive.Zip.extract
    ~archive:(Path.v "bundle.zip")
    ~into:(Path.v "_tmp/bundle")
  |> Result.expect ~msg:"Failed to extract zip archive"
```

This RFD does not require every archive/compression module to share the same
I/O surface.

The intended split is:

- `Std.Compress.{Flate,Zlib,Gzip}` should expose streaming APIs over
  `IO.Reader` / `IO.Writer`
- `Std.Archive.Tar` should expose streaming APIs over `IO.Reader` / `IO.Writer`
- `Std.Archive.Zip` should remain file/seek oriented in v1 rather than
  pretending zip is a pure forward-only stream format

Path-based and file-based convenience helpers should still exist in `std`, but
they should be wrappers over the streaming APIs for tar/gzip/zlib/flate.

So the user model is:

- streaming `Reader` / `Writer` APIs for tar and compression codecs
- file/path helpers layered on top of those APIs
- file/seek oriented zip APIs in v1
- resumable engines in `kernel`

### Security model

Archive extraction must be safe by default.
`Std.Archive.Tar.extract` and `Std.Archive.Zip.extract` should reject entries
that would escape the requested destination or introduce unsafe filesystem
objects.

At minimum, extraction should reject:

- absolute paths
- normalized paths containing `..`
- duplicate normalized output paths
- block devices, character devices, fifos, and sockets

Symlinks and hardlinks should not be silently extracted in the first version.
They should either be rejected or be handled only behind an explicit future API.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Proposed package/module structure

The new module layout should mirror the namespace split in both `kernel` and
`std`.

In `kernel`:

- `Kernel.Archive`
  - `Kernel.Archive.Tar`
  - `Kernel.Archive.Zip`
- `Kernel.Compress`
  - `Kernel.Compress.Flate`
  - `Kernel.Compress.Zlib`
  - `Kernel.Compress.Gzip`

In `std`:

- `Std.Archive`
  - `Std.Archive.Tar`
  - `Std.Archive.Zip`
- `Std.Compress`
  - `Std.Compress.Flate`
  - `Std.Compress.Zlib`
  - `Std.Compress.Gzip`

`kernel` owns the foreign bindings and low-level result translation.
`std` owns the ergonomic path/entry APIs and the stable Riot-facing names.

### Native dependency strategy

This RFD standardizes one external native dependency:

- `zlib`

`zlib` is required at the `kernel` boundary for:

- deflate/inflate
- zlib framing
- gzip framing
- CRC32 required by gzip/zip validation

Archive handling for tar and zip should live in native `kernel` code as well,
but the specific internal implementation technique is not part of this RFD's
external dependency contract.

That means:

- Riot commits to `zlib` as a system/toolchain dependency
- Riot does not commit this RFD to any additional external archive library
- tar/zip implementation details stay behind the `kernel` boundary as long as
  the public `Kernel` and `Std` APIs remain stable

### Kernel boundary

The `kernel` surface should remain narrow and mechanical.
It should not expose Riot-specific policy such as extraction destination layout
or workspace-relative path rewriting.

It should also remain incremental.
The `kernel` layer must not expose monolithic functions such as:

- `extract_archive_to_dir`
- `create_archive_from_dir`
- `compress_file`
- `decompress_file`

Those operations are too large and too scheduler-hostile for Riot's runtime.

For compression, the `kernel` layer should expose resumable engines:

- encoder/decoder state
- bounded encode/decode steps over caller-provided input/output buffers
- codec-specific errors
- checksum mismatch reporting where applicable

For archives, the `kernel` layer should expose resumable archive readers and
writers:

- archive entry iteration
- entry metadata
- bounded payload read/write steps
- typed failure reasons where the backing library provides them

The intent is to keep `kernel` close to the underlying native behavior and let
`std` enforce the higher-level Riot rules while driving I/O through Riot's own
file and reader/writer abstractions.

### Kernel engine model

The compression boundary should look like an engine API, not a whole-file API.

Representative shape:

```ocaml
module Kernel.Compress.Flate : sig
  type encoder
  type decoder
  type error

  type flush =
    [ `None
    | `Sync
    | `Finish ]

  type status =
    [ `Need_input
    | `Need_output
    | `Finished ]

  type step = {
    consumed: int;
    produced: int;
    status: status;
  }

  val create_encoder: ?level:int -> unit -> (encoder, error) result
  val create_decoder: unit -> (decoder, error) result

  val encode:
    encoder ->
    src:bytes -> src_pos:int -> src_len:int ->
    dst:bytes -> dst_pos:int -> dst_len:int ->
    flush:flush ->
    (step, error) result

  val decode:
    decoder ->
    src:bytes -> src_pos:int -> src_len:int ->
    dst:bytes -> dst_pos:int -> dst_len:int ->
    (step, error) result

  val reset_encoder: encoder -> (unit, error) result
  val reset_decoder: decoder -> (unit, error) result
  val close_encoder: encoder -> unit
  val close_decoder: decoder -> unit
end
```

`Kernel.Compress.Zlib` and `Kernel.Compress.Gzip` should mirror this style.

The tar boundary should be stream-first and resumable:

```ocaml
module Kernel.Archive.Tar : sig
  type reader
  type writer
  type error

  type entry_kind =
    [ `File
    | `Directory
    | `Symlink
    | `Hardlink
    | `Other of string ]

  type header = {
    path: string;
    kind: entry_kind;
    size: int64;
    mode: int option;
    link_target: string option;
  }

  type next =
    [ `Need_input
    | `Entry of header
    | `End ]

  val create_reader: unit -> (reader, error) result
  val feed_reader: reader -> src:bytes -> src_pos:int -> src_len:int -> (int, error) result
  val next_entry: reader -> (next, error) result
  val read_entry_data:
    reader ->
    dst:bytes -> dst_pos:int -> dst_len:int ->
    ([ `Need_input | `Chunk of int | `End_of_entry ], error) result
  val skip_entry: reader -> ([ `Need_input | `Skipped ], error) result
  val close_reader: reader -> unit
end
```

Zip is different.
Tar is naturally streaming, but zip is central-directory oriented and therefore
not purely stream-first for normal access patterns.
The zip kernel boundary should still be incremental, but it will likely be
seekable-file oriented rather than sequential-stream oriented.

### Std surface

The public `std` surface should start small, but for tar and compression codecs
it should explicitly embrace Riot's existing `IO.Reader` / `IO.Writer`
interfaces.

That means:

- `Std.Compress.{Flate,Zlib,Gzip}` should be stream-oriented in v1
- `Std.Archive.Tar` should be stream-oriented in v1
- `Std.Archive.Zip` may remain path- and file-oriented in v1 because zip access
  is naturally seek-based

Representative shapes:

```ocaml
module Std.Archive.Tar : sig
  type entry_kind =
    [ `File
    | `Directory
    | `Symlink
    | `Hardlink
    | `Other of string ]

  type entry = {
    path: Path.t;
    kind: entry_kind;
    size: int64;
    mode: Fs.Permissions.t option;
    mtime: Time.SystemTime.t option;
    link_target: Path.t option;
  }

  type error

  type ('src, 'read_err) source = ('src, 'read_err) IO.Reader.t
  type ('dst, 'write_err) sink = ('dst, 'write_err) IO.Writer.t

  val entries: ('src, 'read_err) source -> (entry list, error) result
  val extract: ('src, 'read_err) source -> into:Path.t -> (unit, error) result
  val create:
    ('dst, 'write_err) sink ->
    root:Path.t ->
    entries:Path.t list ->
    (unit, error) result

  val entries_file: Path.t -> (entry list, error) result
  val extract_file: archive:Path.t -> into:Path.t -> (unit, error) result
  val create_file: output:Path.t -> root:Path.t -> entries:Path.t list -> (unit, error) result
end
```

```ocaml
module Std.Archive.Zip : sig
  type compression_method =
    [ `Stored
    | `Deflated
    | `Unsupported of int ]

  type entry = {
    path: Path.t;
    compressed_size: int64;
    uncompressed_size: int64;
    method_: compression_method;
    is_directory: bool;
    crc32: int32 option;
  }

  type error

  val entries: Path.t -> (entry list, error) result
  val extract: archive:Path.t -> into:Path.t -> (unit, error) result
  val create: output:Path.t -> root:Path.t -> entries:Path.t list -> (unit, error) result
end
```

```ocaml
module Std.Compress.Gzip : sig
  type error

  type ('src, 'read_err) source = ('src, 'read_err) IO.Reader.t
  type ('dst, 'write_err) sink = ('dst, 'write_err) IO.Writer.t

  val compress: src:('src, 'read_err) source -> dst:('dst, 'write_err) sink -> (unit, error) result
  val decompress: src:('src, 'read_err) source -> dst:('dst, 'write_err) sink -> (unit, error) result

  val compress_file: src:Path.t -> dst:Path.t -> (unit, error) result
  val decompress_file: src:Path.t -> dst:Path.t -> (unit, error) result
  val compress_string: string -> (string, error) result
  val decompress_string: string -> (string, error) result
end
```

`Std.Compress.Zlib` and `Std.Compress.Flate` should have the same streaming
shape as `Std.Compress.Gzip`, with file/string helpers layered on top.

But `std` convenience functions must be layered roughly like this:

- `Std.Compress.*`:
  - read input in chunks with `IO.Reader`
  - call kernel encode/decode steps
  - write output in chunks with `IO.Writer`
  - provide `*_file` and `*_string` helpers as wrappers
- `Std.Archive.Tar`:
  - read archive bytes incrementally through `IO.Reader`
  - pull headers and entry payload chunks from the tar reader
  - enforce Riot extraction policy in `std`
  - provide file/path helpers as wrappers
- `Std.Archive.Zip`:
  - read central-directory/footer data with Riot file I/O
  - parse entry metadata incrementally
  - decompress entry payloads through `Kernel.Compress.Flate`

### Extraction safety and normalization

`std` should enforce the extraction policy, even if the native library is more
permissive.

That means the `Std.Archive` modules should:

- normalize entry paths before writing them into the destination directory
- reject path traversal and absolute paths
- fail on duplicate normalized output paths
- reject unsupported filesystem object kinds by default

Archive creation should also normalize entry paths relative to the requested
root and reject entries outside that root.

### Initial format scope

The initial scope should be intentionally conservative.

`Std.Archive.Tar`:

- regular files
- directories
- symlinks optionally listed but not necessarily extracted in v1

`Std.Archive.Zip`:

- stored entries
- deflated entries
- standard central directory handling
- no encryption in v1
- no zip64 requirement in v1

The implementation model should be seekable-file oriented rather than pretending
zip is a pure forward-only stream format.

`Std.Compress.Gzip`:

- standard gzip compress/decompress
- CRC mismatch surfaced as a typed failure

`Std.Compress.Zlib` / `Std.Compress.Flate`:

- byte-oriented compress/decompress
- explicit failure when the input stream is invalid or truncated

### Toolchain and cross-compilation implications

This proposal has a direct effect on Riot's toolchain packaging.

Because Riot links against system `zlib`, then:

- native macOS builds must locate headers and link flags for `zlib`
- native Linux builds must do the same
- packaged cross-compilation toolchains must carry headers and libraries for the
  target sysroot

That means any implementation work must include:

- `packages/kernel/tusk.toml` link flag updates
- toolchain packaging updates so cross sysroots include the required artifacts
- verification on macOS and Linux, including cross targets

### Migration plan

The rollout should happen in phases.

#### Phase 1 - native dependency and kernel plumbing

- choose the native library strategy
- add the required headers/libs to Riot toolchains
- add incremental `kernel` modules for archive/compression
- validate that the native boundary is resumable rather than monolithic

#### Phase 2 - stable `std` surface

- add `Std.Archive.Tar`
- add `Std.Archive.Zip`
- add `Std.Compress.Gzip`
- add `Std.Compress.Zlib`
- add `Std.Compress.Flate`
- implement `IO.Reader` / `IO.Writer` streaming APIs for tar/gzip/zlib/flate
- implement file/path convenience helpers as chunked loops over those streaming
  APIs and the kernel engines
- keep zip file/seek oriented in v1

#### Phase 3 - first consumer migrations

- replace `pkgs-ml` tar shell-outs with `Std.Archive.Tar`
- use `Std.Compress.Gzip` where Riot currently treats `.tar.gz` artifacts as
  opaque bytes

#### Phase 4 - broader package/tooling adoption

- switch publication/install paths to Riot-owned archive creation/extraction
- remove shell dependencies on `tar`, `gzip`, or `unzip` from normal package
  flows

## Drawbacks
[drawbacks]: #drawbacks

- New native dependencies increase toolchain and cross-compilation complexity.
- Even a single new dependency such as `zlib` increases toolchain and
  cross-compilation complexity.
- The incremental engine design is more involved than binding one whole-file or
  whole-archive C function.
- The `std` surface will intentionally be asymmetric in v1:
  tar and compression codecs are stream-oriented, while zip remains
  file/seek-oriented.
- Archive extraction has security footguns, so the policy surface must be
  designed carefully.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why this namespacing

`Std.Archive.{Tar,Zip}` and `Std.Compress.{Gzip,Zlib,Flate}` matches the
semantic split of the underlying formats.

Putting `Zip` under `Compress` would blur archive structure with compression.
Putting `Gzip` under `Archive` would blur a compression container with archive
format semantics it does not have.

### Why native libraries instead of pure OCaml

This RFD explicitly chooses C bindings over pure OCaml implementations.

The reasons are:

- better alignment with Riot's existing `kernel` role
- less reimplementation of well-known low-level codecs
- easier access to mature checksum and format validation behavior
- lower maintenance burden for complex formats such as zip and gzip

### Why not shell out

Shelling out to `tar`, `gzip`, or `unzip` is simple initially, but it keeps
Riot dependent on external tools at runtime and leaks command-line behavior into
the standard library boundary.

That is acceptable as a stopgap.
It is not the right long-term design for `std`.

### Why not only expose one generic archive API

Tar and zip have meaningfully different metadata, compression behavior, and
error modes.
Flattening them into one generic API too early would erase useful semantics and
make the surface harder to understand.

### Alternatives considered

- Pure OCaml tar/zip/gzip implementations.
  Rejected by design in this RFD.

- Only expose `Std.Archive` and hide compression details.
  Rejected because gzip/zlib/flate are independently useful and not archives.

- Shell out to `tar`, `gzip`, or `unzip` permanently.
  Rejected because Riot should own this capability inside `std`/`kernel`.

- Expose monolithic C helpers such as `extract archive` or `compress file`.
  Rejected because they do not fit Riot's cooperative runtime model.

## Prior art
[prior-art]: #prior-art

Go uses this exact conceptual split:

- `archive/tar`
- `archive/zip`
- `compress/gzip`
- `compress/zlib`
- `compress/flate`

Erlang is flatter in naming, but the underlying split is similar:

- `erl_tar`
- `zip`
- `zlib`

Within Riot itself, the most relevant prior art is the current shell-based
archive handling in `packages/pkgs-ml/src/registry.ml`,
which demonstrates the immediate need for tar support but also shows the limits
of keeping archive logic outside Riot's own library surface.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should `Std.Archive` v1 reject symlinks entirely, or allow listing them while
  still rejecting extraction by default?
- Should `Std.Compress.Flate` be public in v1, or kept internal until a second
  consumer appears?
- What is the minimal seekable/file-access helper `Std.Archive.Zip` needs in v1
  without introducing a broader random-access `IO` abstraction too early?

## Future possibilities
[future-possibilities]: #future-possibilities

Once the file-oriented v1 API exists, Riot can add more advanced features
without breaking the initial namespace:

- streaming readers/writers for large archives
- zip64 support
- richer archive metadata preservation
- configurable extraction policy
- use in package publication and installation flows without shelling out
- optional HTTP content-encoding support built on `Std.Compress.Gzip` or
  `Std.Compress.Zlib`
