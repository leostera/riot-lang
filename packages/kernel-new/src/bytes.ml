open Prelude

type t = bytes

let create = Primitives.bytes_create

let length = Primitives.bytes_length

let get = Primitives.bytes_get

let set = Primitives.bytes_set

let blit = Primitives.bytes_blit

let fill = Primitives.bytes_fill

let of_string = Primitives.bytes_of_string

let to_string = Primitives.bytes_to_string

let unsafe_of_string = Primitives.bytes_of_string

let unsafe_to_string = Primitives.bytes_to_string

let sub = fun source offset len ->
  let out = create len in
  blit source offset out 0 len;
  out

let sub_string = fun source offset len -> to_string (sub source offset len)
