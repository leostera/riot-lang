module Iovec: sig
  module IoSlice: sig
    type t

    val create: size:int -> t

    val length: t -> int

    val get: t -> at:int -> char

    val blit_from_bytes:
      bytes ->
      src_offset:int ->
      dst:t ->
      dst_offset:int ->
      len:int ->
      unit

    val blit:
      src:t ->
      src_offset:int ->
      dst:t ->
      dst_offset:int ->
      len:int ->
      unit

    val blit_to_bytes: t -> dst:bytes -> dst_offset:int -> unit

    val blit_from_string:
      string ->
      src_offset:int ->
      dst:t ->
      dst_offset:int ->
      len:int ->
      unit

    val sub: t -> offset:int -> len:int -> t

    val to_string: t -> string
  end

  type segment = IoSlice.t
  type t
  val create: ?count:int -> size:int -> unit -> t

  val with_capacity: int -> t

  val from_slices: segment array -> t

  val from_bytes: bytes -> t

  val from_string: string -> t

  val from_bytes_array: bytes array -> t

  val from_string_array: string array -> t

  val length: t -> int

  val for_each: fn:(segment -> unit) -> t -> unit

  val sub: ?pos:int -> len:int -> t -> t

  val to_bytes: t -> bytes

  val to_string: t -> string
end

module Buffer: sig
  module IoSlice = Iovec.IoSlice

  type t

  val create: ?size:int -> unit -> t

  val length: t -> int

  val capacity: t -> int

  val clear: t -> unit

  val append_string: t -> string -> unit

  val append_bytes: t -> bytes -> unit

  val append_slice: t -> IoSlice.t -> unit

  val writable_slice: ?size:int -> t -> IoSlice.t

  val commit_write: t -> len:int -> unit

  val consume: t -> len:int -> unit

  val readable_slice: t -> IoSlice.t

  val to_iovec: t -> Iovec.t

  val to_bytes: t -> bytes

  val to_string: t -> string
end

module StringView: sig
  module IoSlice = Iovec.IoSlice

  type t

  val empty: t

  val of_slice: IoSlice.t -> t

  val of_string: string -> t

  val of_buffer: Buffer.t -> t

  val length: t -> int

  val get: t -> at:int -> char

  val sub: t -> offset:int -> len:int -> t

  val advance: t -> by:int -> t

  val starts_with: t -> prefix:string -> bool

  val index_of_char: t -> char -> int option

  val index_of_string: t -> string -> int option

  val to_string: t -> string
end

module Stdin: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val read: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val read_vectored: Iovec.t -> (int, error) Result.t

  val to_source: unit -> Async.Source.t
end

module Stdout: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val write: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val write_vectored: Iovec.t -> (int, error) Result.t

  val print: string -> (unit, error) Result.t

  val println: string -> (unit, error) Result.t

  val flush: unit -> (unit, error) Result.t

  val to_source: unit -> Async.Source.t
end

module Stderr: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val write: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val write_vectored: Iovec.t -> (int, error) Result.t

  val print: string -> (unit, error) Result.t

  val println: string -> (unit, error) Result.t

  val flush: unit -> (unit, error) Result.t

  val to_source: unit -> Async.Source.t
end

val print: string -> (unit, Stdout.error) Result.t

val println: string -> (unit, Stdout.error) Result.t

val eprint: string -> (unit, Stderr.error) Result.t

val eprintln: string -> (unit, Stderr.error) Result.t
