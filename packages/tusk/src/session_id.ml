(** Session ID module - provides opaque session identifiers *)

type t = string

(* fixme: use an atomic int and increment it on every new id, so we can keep the id an int until we need to turn it into a string *)
let make () = Printf.sprintf "build-%d-%d" (Unix.getpid ()) (Random.int 1000000)
let to_string t = t
let of_string s = s
