(** Core hash type and operations *)

type t = bytes

let of_bytes bytes = bytes
let to_bytes h = h
let length h = Bytes.length h
let equal a b = Bytes.equal a b
let compare a b = Bytes.compare a b
