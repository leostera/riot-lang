(**
   Read PostgreSQL wire values from a byte buffer.

   Use `Binary_reader` when decoding fields from PostgreSQL's binary protocol.
   The reader keeps an internal cursor so higher-level decoders can pull
   values in sequence without managing offsets manually.
*)
type t

(**
   Create a reader over a byte buffer.

   The initial position is `0`.
*)
val create: bytes -> t

(** Return the number of unread bytes remaining in the buffer. *)
val remaining: t -> int

(** Return `true` when the reader has reached the end of the buffer. *)
val is_eof: t -> bool

(**
   Read one byte and advance the cursor.

   Returns `None` when there is no data left to read.
*)
val read_byte: t -> int option

(** Read a 32-bit integer from the current position. *)
val read_int32: t -> int option

(** Read a 16-bit integer from the current position. *)
val read_int16: t -> int option

(** Read a 64-bit integer from the current position. *)
val read_int64: t -> int64 option

(** Read an IEEE 754 double-precision float from the current position. *)
val read_float64: t -> float option

(** Read an IEEE 754 single-precision float from the current position. *)
val read_float32: t -> float option

(**
   Read the remaining bytes as a string.

   Returns `None` when the remaining payload cannot be decoded as a string.
*)
val read_string: t -> string option

(**
   Read exactly `length` bytes.

   Returns `None` when fewer than `length` bytes remain.
*)
val read_bytes: t -> int -> bytes option

(**
   Read a fixed-width C string.

   Use this for PostgreSQL fields encoded as null-terminated strings inside a
   known-width region.
*)
val read_cstring: t -> int -> string option

(** Return the current read position in bytes. *)
val position: t -> int

(** Move the reader cursor to an absolute byte offset. *)
val set_position: t -> int -> unit
