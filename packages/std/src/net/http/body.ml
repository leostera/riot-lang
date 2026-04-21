type t =
  | String of string
  | Slice of IO.IoVec.IoSlice.t

let empty = String ""

let from_string = fun value -> String value

let from_slice = fun value -> Slice value

let length = function
  | String value -> String.length value
  | Slice value -> IO.IoVec.IoSlice.length value

let is_empty = fun value ->
  Int.equal (length value) 0

let to_string = function
  | String value -> value
  | Slice value -> IO.IoVec.IoSlice.to_string value

let to_slice_opt = function
  | String _ -> None
  | Slice value -> Some value
