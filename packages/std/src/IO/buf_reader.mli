open Prelude
open Types

module IoSlice = IoSlice

type 'err t

type 'err error =
  | Source_error of 'err
  | End_of_file
  | Buffer_full
  | Invalid_count of int
  | Invalid_data

val from_reader:
  ?size:int ->
  'err Reader.t ->
  'err t

val to_reader: 'err t -> 'err error Reader.t

val read:
  'err t ->
  into:Buffer.t ->
  (int, 'err error) Result.t

val read_byte:
  'err t ->
  (u8, 'err error) Result.t

val size: 'err t -> int

val reset:
  'err t ->
  reader:'err Reader.t ->
  unit

val fill:
  'err t ->
  (int, 'err error) Result.t

val peek:
  'err t ->
  len:int ->
  (IoSlice.t, 'err error) Result.t

val consume:
  'err t ->
  len:int ->
  (int, 'err error) Result.t

val read_rune:
  'err t ->
  (Kernel.Unicode.Rune.t, 'err error) Result.t

val read_slice:
  'err t ->
  until:u8 ->
  (IoSlice.t, 'err error) Result.t

val read_line:
  'err t ->
  (IoSlice.t, 'err error) Result.t

val read_string:
  'err t ->
  until:u8 ->
  (string, 'err error) Result.t
