open Global0

include Stdlib.List

let make ~len ~fn =
  let rec build acc i =
    if i >= len then rev acc
    else build (fn i :: acc) (i + 1)
  in
  build [] 0

let unique lst =
  let rec aux seen = function
    | [] -> []
    | x :: xs ->
        if exists (fun y -> x = y) seen then
          aux seen xs
        else
          x :: aux (x :: seen) xs
  in
  aux [] lst
