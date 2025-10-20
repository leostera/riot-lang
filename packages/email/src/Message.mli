open Std

type t

val of_string : string -> (t, string) Result.t
val to_string : t -> string
val headers : t -> (string * string) List.t
val body : t -> string
val make : headers:(string * string) List.t -> body:string -> t
val to_json : t -> Data.Json.t
