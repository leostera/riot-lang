open Prelude

type t = string

let empty = ""

let length = Caml_runtime.string_length

let get = Caml_runtime.string_get

let init = fun length builder ->
  let out = Caml_runtime.bytes_create length in
  let rec fill index =
    if index >= length then
      out
    else (
      Caml_runtime.bytes_set out index (builder index);
      fill (index + 1)
    )
  in
  let _ = fill 0 in
  Caml_runtime.bytes_unsafe_to_string out

let make = fun length char -> init length (fun _ -> char)

let append = fun left right ->
  let left_length = length left in
  let right_length = length right in
  let out = Caml_runtime.bytes_create (left_length + right_length) in
  Caml_runtime.string_blit left 0 out 0 left_length;
  Caml_runtime.string_blit right 0 out left_length right_length;
  Caml_runtime.bytes_unsafe_to_string out

let concat = fun separator values ->
  let rec total_length acc = function
    | [] -> acc
    | [ value ] -> acc + length value
    | value :: rest -> total_length (acc + length value + length separator) rest
  in
  let rec fill out offset = function
    | [] ->
        out
    | [ value ] ->
        let value_length = length value in
        Caml_runtime.string_blit value 0 out offset value_length;
        out
    | value :: rest ->
        let value_length = length value in
        let separator_length = length separator in
        Caml_runtime.string_blit value 0 out offset value_length;
        Caml_runtime.string_blit separator 0 out (offset + value_length) separator_length;
        fill out (offset + value_length + separator_length) rest
  in
  match values with
  | [] ->
      empty
  | [ value ] ->
      value
  | values ->
      let out = Caml_runtime.bytes_create (total_length 0 values) in
      Caml_runtime.bytes_unsafe_to_string (fill out 0 values)

let equal = Caml_runtime.equal

let compare = Caml_runtime.compare

let of_bytes = Caml_runtime.bytes_to_string

let to_bytes = Caml_runtime.bytes_of_string
