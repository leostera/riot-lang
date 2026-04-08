open Prelude

type 'a t = 'a array

let make = Primitives.array_make

let init = fun length builder ->
  if length = 0 then
    [||]
  else
    let out = make length (builder 0) in
    for index = 1 to length - 1 do
      Primitives.array_set out index (builder index)
    done;
    out

let length = Primitives.array_length

let get = Primitives.array_get

let set = Primitives.array_set

let iter = fun fn array ->
  for index = 0 to length array - 1 do
    fn (get array index)
  done

let map = fun fn array ->
  let count = length array in
  if count = 0 then
    [||]
  else
    let out = make count (fn (get array 0)) in
    for index = 1 to count - 1 do
      set out index (fn (get array index))
    done;
    out

let fold_left = fun fn init array ->
  let rec loop index acc =
    if index >= length array then
      acc
    else
      loop (index + 1) (fn acc (get array index))
  in
  loop 0 init

let of_list = function
  | [] -> [||]
  | head :: tail ->
      let rec list_length acc =
        function
        | [] -> acc
        | _ :: rest -> list_length (acc + 1) rest
      in
      let out = make (list_length 1 tail) head in
      let rec fill index =
        function
        | [] -> out
        | value :: rest ->
            set out index value;
            fill (index + 1) rest
      in
      fill 1 tail
