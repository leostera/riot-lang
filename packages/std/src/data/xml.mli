type t =
  | Element of {
      name : string;
      attrs : (string * string) list;
      children : t list;
    }
  | Text of string
  | CData of string

val element : string -> ?attrs:(string * string) list -> t list -> t
val text : string -> t
val cdata : string -> t
val to_string : ?indent:int -> t -> string
val declaration : string
