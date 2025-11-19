open Std

type t = string list

val empty : t
val separator : string
val of_string : string -> t
val of_list : string list -> t
val append : t -> string -> t
val to_string : t -> string
val to_list : t -> string list
val is_empty : t -> bool
val from_path : Path.t -> t
