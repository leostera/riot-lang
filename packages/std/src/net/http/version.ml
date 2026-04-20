open Global
module Slice = IO.IoVec.IoSlice

type t =
  Http09
  | Http10
  | Http11
  | Http2
  | Http3

let of_string = function
  | "HTTP/0.9" -> Ok Http09
  | "HTTP/1.0" -> Ok Http10
  | "HTTP/1.1" -> Ok Http11
  | "HTTP/2"
  | "HTTP/2.0" -> Ok Http2
  | "HTTP/3"
  | "HTTP/3.0" -> Ok Http3
  | _ -> Error `InvalidVersion

let equal_tail = fun value suffix ->
  let suffix_len = String.length suffix in
  if Slice.length value != suffix_len then
    false
  else
    let rec loop index =
      if index >= suffix_len then
        true
      else if Slice.get_unchecked value ~at:index = String.get_unchecked suffix ~at:index then
        loop (index + 1)
      else
        false
    in
    loop 0

let from_slice = fun value ->
  match Slice.length value with
  | 6 ->
      if equal_tail value "HTTP/2" then
        Ok Http2
      else if equal_tail value "HTTP/3" then
        Ok Http3
      else
        Error `InvalidVersion
  | 8 ->
      if equal_tail value "HTTP/0.9" then
        Ok Http09
      else if equal_tail value "HTTP/1.0" then
        Ok Http10
      else if equal_tail value "HTTP/1.1" then
        Ok Http11
      else if equal_tail value "HTTP/2.0" then
        Ok Http2
      else if equal_tail value "HTTP/3.0" then
        Ok Http3
      else
        Error `InvalidVersion
  | _ -> Error `InvalidVersion

let to_string = function
  | Http09 -> "HTTP/0.9"
  | Http10 -> "HTTP/1.0"
  | Http11 -> "HTTP/1.1"
  | Http2 -> "HTTP/2"
  | Http3 -> "HTTP/3"

let compare = fun v1 v2 ->
  let version_num = function
    | Http09 -> 0
    | Http10 -> 1
    | Http11 -> 2
    | Http2 -> 3
    | Http3 -> 4
  in
  Int.compare (version_num v1) (version_num v2)

let equal = fun v1 v2 -> compare v1 v2 = 0

let is_supported = function
  | Http09
  | Http10
  | Http11 -> true
  | Http2
  | Http3 -> false
