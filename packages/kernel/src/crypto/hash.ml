(** Core hash type and operations *)
open IO

type t = bytes

let of_bytes = fun bytes -> bytes

let to_bytes = fun h -> h

let length = fun h -> Bytes.length h

let equal = fun a b ->
    Bytes.equal a b

let compare = fun a b ->
    Bytes.compare a b
