open Prelude

type ('src, 'err) t

val of_reader:
  ?chunk_size:int ->
  ('src, 'err) Reader.t ->
  ('src, 'err) t

val to_reader: ('src, 'err) t -> (('src, 'err) t, 'err) Reader.t

val read:
  ('src, 'err) t ->
  ?timeout:int64 ->
  bytes ->
  (int, 'err) result

val read_vectored:
  ('src, 'err) t ->
  Kernel.IO.Iovec.t ->
  (int, 'err) result

val read_char: ('src, 'err) t -> (char option, 'err) result

val read_line: ('src, 'err) t -> (string, 'err) result

val read_to_string:
  ('src, 'err) t ->
  len:int ->
  (string, 'err) result

(** [peek_slice reader] returns the current readable borrowed bytes.

    The returned slice is only valid until the next [read], [read_vectored], [read_char],
    [read_line], [read_to_string], [peek_slice], [read_slice], [read_line_slice], or [consume]
    on the same buffered reader. *)
val peek_slice:
  ('src, 'err) t ->
  (Kernel.IO.Iovec.IoSlice.t option, 'err) result

(** [consume reader ~len] advances the buffered window by [len] bytes. *)
val consume:
  ('src, 'err) t ->
  len:int ->
  unit

(** [read_slice reader ~delim] returns a borrowed slice ending at [delim], or the remaining tail
    at EOF if no delimiter is found.

    The returned slice is only valid until the next buffered-reader operation that may refill or
    consume the internal buffer. *)
val read_slice:
  ('src, 'err) t ->
  delim:char ->
  (Kernel.IO.Iovec.IoSlice.t option, 'err) result

val read_line_slice:
  ('src, 'err) t ->
  (Kernel.IO.Iovec.IoSlice.t option, 'err) result
