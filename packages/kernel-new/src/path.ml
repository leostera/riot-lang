open Prelude

type t = string

let v = fun path -> path

let of_string = fun path -> Result.Ok path

let to_string = fun path -> path

let join = fun left right ->
  match (left, right) with
  | "", path
  | path, "" -> path
  | left, right when String.get left (String.length left - 1) = '/' ->
      String.append left right
  | left, right -> String.append (String.append left "/") right

let ( / ) = join
