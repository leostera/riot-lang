open Prelude

type ('src, 'err) t = (('src, 'err) Reader.buffered, 'err) Reader.t

val of_reader:
  ?chunk_size:int ->
  ('src, 'err) Reader.t ->
  ('src, 'err) t

val to_reader: ('src, 'err) t -> (('src, 'err) Reader.buffered, 'err) Reader.t

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
