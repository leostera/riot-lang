open Std

type t

val from_string: string -> t

val to_string: t -> string

val equal: t -> t -> bool

val compare: t -> t -> Order.t

val has_package_name: t -> bool

val qualify: package_name:string -> t -> t

val split: default_package:string -> t -> string * string

val package_name: default_package:string -> t -> string

val local_id: t -> string
