open Prelude

type t = int

let make = fun value ->
  if value = 0 then
    Option.None
  else
    Option.Some value
