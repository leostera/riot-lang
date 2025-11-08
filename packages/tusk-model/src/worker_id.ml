(** Opaque worker ID type for type safety *)
open Std
open Std.Collections

type t = int

let make id = id
let to_string id = Int.to_string id
let to_int id = id
let equal = Int.equal
let compare = Int.compare
