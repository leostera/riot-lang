(** Opaque worker ID type for type safety *)

type t = int

let make id = id
let to_string id = string_of_int id
let to_int id = id
let equal = Int.equal
let compare = Int.compare
