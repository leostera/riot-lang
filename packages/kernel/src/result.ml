open Prelude

type ('value, 'error) t = ('value, 'error) result =
  | Ok of 'value
  | Error of 'error

let map = fun fn ->
  function
  | Ok value -> Ok (fn value)
  | Error error -> Error error

let map_error = fun fn ->
  function
  | Ok value -> Ok value
  | Error error -> Error (fn error)

let and_then = fun value next ->
  match value with
  | Ok value -> next value
  | Error error -> Error error
