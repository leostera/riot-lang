(** Opaque worker ID type for type safety *)
open Std
open Std.Collections

type t = int

let make = fun id -> id

let to_string = fun id -> Int.to_string id

let to_int = fun id -> id

let equal = Int.equal

let compare = Int.compare
