open Prelude

type t = int

let make = fun value ->
  if value = 0 then
    None
  else
    Some value
