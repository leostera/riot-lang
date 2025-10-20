open Std

type t

val of_string : string -> (t, string) Result.t
val to_string : t -> string
val display_name : t -> string Option.t
val local_part : t -> string
val domain : t -> string
val address : t -> string
val make : ?display_name:string -> local_part:string -> domain:string -> t
