open Prelude

type t = bytes

let create = Caml_runtime.bytes_create

let length = Caml_runtime.bytes_length

let get = Caml_runtime.bytes_get

let set = Caml_runtime.bytes_set

let blit = Caml_runtime.bytes_blit

let fill = Caml_runtime.bytes_fill

let of_string = fun value ->
  let length = Caml_runtime.string_length value in
  let out = create length in
  Caml_runtime.string_blit value 0 out 0 length;
  out

let to_string = fun value ->
  let length = length value in
  let out = create length in
  blit value 0 out 0 length;
  Caml_runtime.bytes_to_string out

let sub = fun source offset len ->
  let out = create len in
  blit source offset out 0 len;
  out

let sub_string = fun source offset len -> to_string (sub source offset len)
