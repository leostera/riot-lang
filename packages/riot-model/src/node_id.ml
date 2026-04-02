(** Node identifier - ensures single source of truth for build nodes *)
open Std

type t = string

let of_package = fun (package: Package.t) -> package.name

let to_string = fun t -> t

let compare = String.compare

let equal = fun a b -> a = b
