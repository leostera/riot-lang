open Global

module Slice = IO.IoVec.IoSlice

type t =
  | Http09
  | Http10
  | Http11
  | Http2
  | Http3

type error =
  | InvalidVersion

let from_string = fun __tmp1 ->
  match __tmp1 with
  | "HTTP/0.9" -> Ok Http09
  | "HTTP/1.0" -> Ok Http10
  | "HTTP/1.1" -> Ok Http11
  | "HTTP/2"
  | "HTTP/2.0" -> Ok Http2
  | "HTTP/3"
  | "HTTP/3.0" -> Ok Http3
  | _ -> Error InvalidVersion

let from_slice = fun value ->
  match Slice.length value with
  | 8 when Slice.equal_string value "HTTP/0.9" -> Ok Http09
  | 8 when Slice.equal_string value "HTTP/1.0" -> Ok Http10
  | 8 when Slice.equal_string value "HTTP/1.1" -> Ok Http11
  | 6 when Slice.equal_string value "HTTP/2" -> Ok Http2
  | 8 when Slice.equal_string value "HTTP/2.0" -> Ok Http2
  | 6 when Slice.equal_string value "HTTP/3" -> Ok Http3
  | 8 when Slice.equal_string value "HTTP/3.0" -> Ok Http3
  | _ -> Error InvalidVersion

let to_string = fun __tmp1 ->
  match __tmp1 with
  | Http09 -> "HTTP/0.9"
  | Http10 -> "HTTP/1.0"
  | Http11 -> "HTTP/1.1"
  | Http2 -> "HTTP/2"
  | Http3 -> "HTTP/3"

let compare = fun v1 v2 ->
  let version_num = fun __tmp1 ->
    match __tmp1 with
    | Http09 -> 0
    | Http10 -> 1
    | Http11 -> 2
    | Http2 -> 3
    | Http3 -> 4
  in
  Int.compare (version_num v1) (version_num v2)

let equal = fun v1 v2 ->
  match compare v1 v2 with
  | Order.EQ -> true
  | Order.LT
  | Order.GT -> false

let is_supported = fun __tmp1 ->
  match __tmp1 with
  | Http09
  | Http10
  | Http11 -> true
  | Http2
  | Http3 -> false
