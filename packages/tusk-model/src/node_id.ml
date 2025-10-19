(** Node identifier - ensures single source of truth for build nodes *)

type t = string

let of_package (package : Package.t) = package.name
let to_string t = t
let compare = String.compare
let equal a b = a = b
let pp fmt t = Format.fprintf fmt "%s" t
