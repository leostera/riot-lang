open Prelude

type t = bytes

type error =
  | OutOfBoundSet of { bytes: bytes; lenght: int; at: int; char: char }

val create: size:int -> t

val length: t -> int

val get: t -> at:int -> char option

val get_unchecked: t -> at:int -> char

val set: t -> at:int -> char:char -> (unit, error) result

val set_unchecked: t -> at:int -> char:char -> unit

(**
   Use `unsafe_set value index char` as the conventional alias for
   `set_unchecked value ~at:index ~char`. 
*)
val unsafe_set: t -> int -> char -> unit

val blit: t -> src_offset:int -> dst:t -> dst_offset:int -> len:int -> (unit, error) result

val blit_unchecked: t -> src_offset:int -> dst:t -> dst_offset:int -> len:int -> unit

val fill: t -> offset:int -> len:int -> char:char -> unit

(** Use `from_string value` to copy `value` into fresh mutable bytes. *)
val from_string: string -> t

(** Use `to_string value` to copy `value` into a fresh immutable string. *)
val to_string: t -> string

(**
   Use `unsafe_to_string value` only when the caller already owns `value` and will not mutate it
   afterward. 
*)
val unsafe_to_string: t -> string

(** Use `sub value offset len` to copy the selected byte slice into fresh mutable bytes. *)
val sub: t -> offset:int -> len:int -> (t, error) result

val sub_unchecked: t -> offset:int -> len:int -> t

(**
   Use `sub_string value offset len` to copy the selected byte slice into a fresh immutable
   string. 
*)
val sub_string: t -> offset:int -> len:int -> string
