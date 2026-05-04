open Std

type t = string

type id = t

let to_string = fun value -> value

let from_string = fun value -> value

let to_json = Data.Json.string

let from_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.String value -> Ok value
  | value -> Error ("Expected string error id, got " ^ Data.Json.to_string value)
