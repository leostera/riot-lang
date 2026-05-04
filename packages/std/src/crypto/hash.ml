open Kernel

type t = bytes

let from_bytes = fun bytes -> Bytes.sub_unchecked bytes ~offset:0 ~len:(Bytes.length bytes)

let to_bytes = fun hash -> Bytes.sub_unchecked hash ~offset:0 ~len:(Bytes.length hash)

let length = Bytes.length

let equal = fun left right ->
  match compare left right with
  | Order.EQ -> true
  | Order.LT
  | Order.GT -> false

let compare = fun left right ->
  let left_length = Bytes.length left in
  let right_length = Bytes.length right in
  let rec loop index =
    if index >= left_length || index >= right_length then
      Int.compare left_length right_length
    else
      let byte_compare =
        Int.compare
          (Char.to_int (Bytes.get_unchecked left ~at:index))
          (Char.to_int (Bytes.get_unchecked right ~at:index))
      in
      match byte_compare with
      | Order.EQ -> loop (index + 1)
      | Order.LT
      | Order.GT -> byte_compare
  in
  loop 0
