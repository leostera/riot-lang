open Std

type t =
  | Element of {
      name : string;
      attrs : (string * string) list;
      children : t list;
    }
  | Text of string
  | Raw of string

val element : string -> ?attrs:(string * string) list -> t list -> t
val text : string -> t
val raw : string -> t
val fragment : t list -> t
val to_string : t -> string
