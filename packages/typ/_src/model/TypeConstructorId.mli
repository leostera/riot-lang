open Std

type t
val compare: t -> t -> int

val equal: t -> t -> bool

val make: owner:string -> local_id:int -> t

val owner: t -> string

val local_id: t -> int

val of_path: SurfacePath.t -> t

val of_int: int -> t

val to_int: t -> int

val to_json: t -> Data.Json.t

val of_json: Data.Json.t -> (t, string) result

val to_string: t -> string
