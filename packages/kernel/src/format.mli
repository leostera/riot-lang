(** Mechanical string assembly helpers for nostdlib packages. *)
type t =
  | String of string
  | Char of char
  | Uchar of Uchar.t
  | Bool of bool
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | Bytes of bytes
val str: string -> t

val char: char -> t

val uchar: Uchar.t -> t

val bool: bool -> t

val int: int -> t

val int32: int32 -> t

val int64: int64 -> t

val float: float -> t

val bytes: bytes -> t

val to_string: t -> string

val format: t list -> string
