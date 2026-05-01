open Prelude

type t =
  | Negative_size of int
  | Negative_offset of int
  | Negative_length of int
  | Invalid_count of int
  | Index_out_of_bounds of { buffer_length: int; at: int }
  | Range_out_of_bounds of { buffer_length: int; offset: int; len: int }
  | Shift_out_of_bounds of { buffer_length: int; by: int }
  | Split_out_of_bounds of { buffer_length: int; at: int }
  | Commit_out_of_bounds of { writable_bytes: int; requested: int }
  | Consume_out_of_bounds of { readable_bytes: int; requested: int }

let message = fun __tmp1 ->
  match __tmp1 with
  | Negative_size size -> "negative size: " ^ Int.to_string size
  | Negative_offset offset -> "negative offset: " ^ Int.to_string offset
  | Negative_length len -> "negative length: " ^ Int.to_string len
  | Invalid_count count -> "invalid count: " ^ Int.to_string count
  | Index_out_of_bounds { buffer_length; at } ->
      "index out of bounds: index=" ^ Int.to_string at ^ ", length=" ^ Int.to_string buffer_length
  | Range_out_of_bounds { buffer_length; offset; len } ->
      "range out of bounds: offset="
      ^ Int.to_string offset
      ^ ", len="
      ^ Int.to_string len
      ^ ", length="
      ^ Int.to_string buffer_length
  | Shift_out_of_bounds { buffer_length; by } ->
      "shift out of bounds: by=" ^ Int.to_string by ^ ", length=" ^ Int.to_string buffer_length
  | Split_out_of_bounds { buffer_length; at } ->
      "split out of bounds: at=" ^ Int.to_string at ^ ", length=" ^ Int.to_string buffer_length
  | Commit_out_of_bounds { writable_bytes; requested } ->
      "commit out of bounds: requested="
      ^ Int.to_string requested
      ^ ", writable="
      ^ Int.to_string writable_bytes
  | Consume_out_of_bounds { readable_bytes; requested } ->
      "consume out of bounds: requested="
      ^ Int.to_string requested
      ^ ", readable="
      ^ Int.to_string readable_bytes
