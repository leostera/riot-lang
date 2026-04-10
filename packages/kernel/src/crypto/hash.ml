(** Core hash type and operations *)
open IO

type t = bytes

let of_bytes: bytes -> t = fun bytes -> bytes

let to_bytes: t -> bytes = fun hash -> hash

let length: t -> int = fun hash -> Bytes.length hash

let equal: t -> t -> bool = fun a b ->
  Bytes.equal a b

let compare: t -> t -> int = fun a b ->
  Bytes.compare a b
