include Stdlib.List

let make ~len ~fn =
  let rec build acc i =
    if i >= len then rev acc
    else build (fn i :: acc) (i + 1)
  in
  build [] 0
