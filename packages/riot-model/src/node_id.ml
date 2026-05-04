(** Node identifier - ensures single source of truth for build nodes *)
open Std

type t = Package_name.t

let from_package = fun (package: Package.t) -> package.name

let to_string = Package_name.to_string

let compare = Package_name.compare

let equal = Package_name.equal
