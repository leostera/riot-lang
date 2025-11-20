open Std

type t = {
  filename : Path.t;
  namespace : Namespace.t;
  name : string;
}

val make : filename:Path.t -> namespace:Namespace.t -> name:string -> t
val simple_name : t -> string

val qualified_name : t -> string
(** Get the qualified name with double underscores (e.g., "Codedb__Model__Symbol").
    This follows Tusk_model conventions and is the OCaml compilation unit name. *)

val namespace_list : t -> string list
val filename : t -> Path.t

val to_string : t -> string
(** Returns the simple name (matches Tusk_model behavior) *)

val from_string : string -> (t, string) result
val of_string_exn : string -> t
val hash : t -> Crypto.hash
val equal : t -> t -> bool
val to_json : t -> Data.Json.t
val from_json : Data.Json.t -> (t, string) result
