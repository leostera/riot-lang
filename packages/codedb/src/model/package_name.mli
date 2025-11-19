open Std

type t = string

val from_string : string -> (t, string) result
val to_string : t -> string
val of_string_exn : string -> t
val to_json : t -> Data.Json.t
val from_json : Data.Json.t -> (t, string) result
