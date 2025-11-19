open Std

type t = { name : Package_name.t; path : Path.t }

val make : name:Package_name.t -> path:Path.t -> t
val to_json : t -> Data.Json.t
val from_json : Data.Json.t -> (t, string) result
