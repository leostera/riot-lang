(** Simple JSON library for RPC communication *)

(** JSON value type *)
type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list

val to_string : t -> string
(** Serialization *)

val of_string : string -> (t, string) result
(** Deserialization *)

val null : t
(** Helper functions for building JSON *)

val bool : bool -> t
val int : int -> t
val float : float -> t
val string : string -> t
val array : t list -> t
val obj : (string * t) list -> t

val get_field : string -> t -> t option
(** Helper functions for extracting values *)

val get_string : t -> string option
val get_int : t -> int option
val get_bool : t -> bool option
val get_array : t -> t list option
val get_object : t -> (string * t) list option
