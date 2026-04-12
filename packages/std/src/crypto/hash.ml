open Kernel

type t = bytes

let of_bytes = fun bytes ->
  Bytes.sub bytes 0 (Bytes.length bytes)

let to_bytes = fun hash ->
  Bytes.sub hash 0 (Bytes.length hash)

let length = Bytes.length

let equal = fun left right -> compare left right = 0

let compare = fun left right ->
  let left_length = Bytes.length left in
  let right_length = Bytes.length right in
  let rec loop index =
    if index >= left_length || index >= right_length then
      Int.compare left_length right_length
    else
      let byte_compare = Int.compare
        (Char.to_int (Bytes.get left index))
        (Char.to_int (Bytes.get right index)) in
      if byte_compare = 0 then
        loop (index + 1)
      else
        byte_compare
  in
  loop 0
