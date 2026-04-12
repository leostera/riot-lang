open Prelude

type ('value, 'error) t = ('value, 'error) result =
  | Ok of 'value
  | Error of 'error

let map = fun value ~fn ->
  match value with
  | Ok value -> Ok (fn value)
  | Error error -> Error error

let map_err = fun value ~fn ->
  match value with
  | Ok value -> Ok value
  | Error error -> Error (fn error)

let and_then = fun value ~fn ->
  match value with
  | Ok value -> fn value
  | Error error -> Error error
