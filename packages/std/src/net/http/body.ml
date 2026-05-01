type t =
  | String of string
  | Slice of IO.IoVec.IoSlice.t

let empty = String ""

let from_string = fun value -> String value

let from_slice = fun value -> Slice value

let length = fun __tmp1 ->
  match __tmp1 with
  | String value -> String.length value
  | Slice value -> IO.IoVec.IoSlice.length value

let is_empty = fun value -> Int.equal (length value) 0

let to_string = fun __tmp1 ->
  match __tmp1 with
  | String value -> value
  | Slice value -> IO.IoVec.IoSlice.to_string value

let to_slice_opt = fun __tmp1 ->
  match __tmp1 with
  | String _ -> None
  | Slice value -> Some value
