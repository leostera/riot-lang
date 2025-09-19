type t = int

let counter = ref 0

let next () =
  let id = !counter in
  counter := !counter + 1;
  id

let eq a b = a = b

let to_string = string_of_int

let to_int t = t