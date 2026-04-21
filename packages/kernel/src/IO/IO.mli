module Error: module type of Error

module IoVec: sig
  module IoSlice: sig
    type t

    type error = Error.t

    val empty: t

    val create: size:int -> (t, error) Result.t

    val length: t -> int

    val sub: t -> off:int -> len:int -> (t, error) Result.t

    val sub_unchecked: t -> off:int -> len:int -> t

    val shift: t -> int -> (t, error) Result.t

    val shift_unchecked: t -> int -> t

    val split_at: t -> int -> ((t * t), error) Result.t

    val split_at_unchecked: t -> int -> t * t

    val get: t -> at:int -> (char, error) Result.t

    val get_unchecked: t -> at:int -> char

    val set: t -> at:int -> char -> (unit, error) Result.t

    val set_unchecked: t -> at:int -> char -> unit

    val blit:
      src:t ->
      src_off:int ->
      dst:t ->
      dst_off:int ->
      len:int ->
      (unit, error) Result.t

    val blit_unchecked:
      src:t ->
      src_off:int ->
      dst:t ->
      dst_off:int ->
      len:int ->
      unit

    val blit_from_bytes:
      bytes ->
      src_off:int ->
      t ->
      dst_off:int ->
      len:int ->
      (unit, error) Result.t

    val blit_from_bytes_unchecked:
      bytes ->
      src_off:int ->
      t ->
      dst_off:int ->
      len:int ->
      unit

    val blit_to_bytes:
      t ->
      src_off:int ->
      bytes ->
      dst_off:int ->
      len:int ->
      (unit, error) Result.t

    val blit_to_bytes_unchecked:
      t ->
      src_off:int ->
      bytes ->
      dst_off:int ->
      len:int ->
      unit

    val blit_from_string:
      string ->
      src_off:int ->
      t ->
      dst_off:int ->
      len:int ->
      (unit, error) Result.t

    val blit_from_string_unchecked:
      string ->
      src_off:int ->
      t ->
      dst_off:int ->
      len:int ->
      unit

    val from_string: ?off:int -> ?len:int -> string -> (t, error) Result.t

    val from_bytes: ?off:int -> ?len:int -> bytes -> (t, error) Result.t

    val starts_with: t -> prefix:string -> bool

    val equal_string: t -> string -> bool

    val index_char: t -> char -> int option

    val index_string: t -> string -> int option

    val to_string: t -> string

    val to_bytes: t -> bytes
  end

  type segment = IoSlice.t
  type t
  type error = Error.t
  val empty: t
  val create: ?count:int -> size:int -> unit -> (t, error) Result.t

  val with_capacity: int -> (t, error) Result.t

  val from_slices: segment array -> t

  val from_bytes: bytes -> (t, error) Result.t

  val from_string: string -> (t, error) Result.t

  val from_bytes_array: bytes array -> (t, error) Result.t

  val from_string_array: string array -> (t, error) Result.t

  val length: t -> int

  val for_each: fn:(segment -> unit) -> t -> unit

  val sub: ?pos:int -> len:int -> t -> (t, error) Result.t

  val to_bytes: t -> bytes

  val to_string: t -> string
end

module Buffer: sig
  module IoSlice = IoVec.IoSlice

  type t
  type error = Error.t

  val create: ?size:int -> unit -> (t, error) Result.t

  val length: t -> int

  val readable_bytes: t -> int

  val capacity: t -> int

  val writable_bytes: t -> int

  val clear: t -> unit

  val compact: t -> unit

  val ensure_free: t -> int -> (unit, error) Result.t

  val readable: t -> IoSlice.t

  val writable: t -> IoSlice.t

  val commit: t -> int -> (unit, error) Result.t

  val consume: t -> len:int -> (unit, error) Result.t

  val append_string: t -> string -> (unit, error) Result.t

  val append_bytes: t -> bytes -> (unit, error) Result.t

  val append_subbytes: t -> bytes -> off:int -> len:int -> (unit, error) Result.t

  val append_substring: t -> string -> off:int -> len:int -> (unit, error) Result.t

  val append_slice: t -> IoSlice.t -> (unit, error) Result.t

  val append_subslice: t -> IoSlice.t -> off:int -> len:int -> (unit, error) Result.t

  val get: t -> at:int -> (char, error) Result.t

  val get_unchecked: t -> at:int -> char

  val to_iovec: t -> IoVec.t

  val to_bytes: t -> bytes

  val to_string: t -> string
end

module Stdin: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val read: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val read_vectored: IoVec.t -> (int, error) Result.t

  val to_source: unit -> Async.Source.t
end

module Stdout: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val write: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val write_vectored: IoVec.t -> (int, error) Result.t

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

  val write_vectored: IoVec.t -> (int, error) Result.t

  val print: string -> (unit, error) Result.t

  val println: string -> (unit, error) Result.t

  val flush: unit -> (unit, error) Result.t

  val to_source: unit -> Async.Source.t
end

val print: string -> (unit, Stdout.error) Result.t

val println: string -> (unit, Stdout.error) Result.t

val eprint: string -> (unit, Stderr.error) Result.t

val eprintln: string -> (unit, Stderr.error) Result.t
