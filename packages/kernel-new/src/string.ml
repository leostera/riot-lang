open Prelude

type t = string

let empty = ""

let length = Primitives.string_length

let get = Primitives.string_get

let init = fun length builder ->
  let out = Primitives.bytes_create length in
  for index = 0 to length - 1 do
    Primitives.bytes_set out index (builder index)
  done;
  Primitives.bytes_to_string out

let make = fun length char ->
  init length (fun _ -> char)

let append = fun left right ->
  let left_length = length left in
  let right_length = length right in
  let out = Primitives.bytes_create (left_length + right_length) in
  Primitives.string_blit left 0 out 0 left_length;
  Primitives.string_blit right 0 out left_length right_length;
  Primitives.bytes_to_string out

let concat = fun separator values ->
  let rec total_length acc =
    function
    | [] -> acc
    | [value] -> acc + length value
    | value :: rest -> total_length (acc + length value + length separator) rest
  in
  let rec fill out offset =
    function
    | [] -> out
    | [value] ->
        let value_length = length value in
        Primitives.string_blit value 0 out offset value_length;
        out
    | value :: rest ->
        let value_length = length value in
        let separator_length = length separator in
        Primitives.string_blit value 0 out offset value_length;
        Primitives.string_blit separator 0 out (offset + value_length) separator_length;
        fill out (offset + value_length + separator_length) rest
  in
  match values with
  | [] -> empty
  | [value] -> value
  | values ->
      let out = Primitives.bytes_create (total_length 0 values) in
      Primitives.bytes_to_string (fill out 0 values)

let equal = Primitives.equal

let compare = Primitives.compare

let of_bytes = Primitives.bytes_to_string

let to_bytes = Primitives.bytes_of_string
