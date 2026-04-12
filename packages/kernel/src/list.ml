open Prelude

type 'value t = 'value list

let length =
  let rec loop count = function
    | [] -> count
    | _ :: rest -> loop (count + 1) rest
  in
  fun values -> loop 0 values

let is_empty = function
  | [] -> true
  | _ -> false

let rec append left right =
  match left with
  | [] -> right
  | value :: rest -> value :: append rest right

let rec reverse_append left right =
  match left with
  | [] -> right
  | value :: rest -> reverse_append rest (value :: right)

let reverse = fun values -> reverse_append values []

let concat =
  let rec loop acc = function
    | [] -> reverse acc
    | values :: rest -> loop (reverse_append values acc) rest
  in
  fun values -> loop [] (reverse values)

let init = fun ~count ~fn ->
  if count < 0 then
    System_error.panic ("List.init received a negative count: " ^ Int.to_string count)
  else
    let rec loop index acc =
      if index < 0 then
        acc
      else
        loop (index - 1) (fn index :: acc)
    in
    loop (count - 1) []

let map = fun values ~fn ->
  let rec loop acc = function
    | [] -> reverse acc
    | value :: rest -> loop (fn value :: acc) rest
  in
  loop [] values

let for_each = fun values ~fn ->
  let rec loop = function
    | [] -> ()
    | value :: rest ->
        fn value;
        loop rest
  in
  loop values

let fold_left = fun values ~acc ~fn ->
  let rec loop acc = function
    | [] -> acc
    | value :: rest -> loop (fn acc value) rest
  in
  loop acc values

let fold_right = fun values ~acc ~fn ->
  let rec loop values acc =
    match values with
    | [] -> acc
    | value :: rest -> fn value (loop rest acc)
  in
  loop values acc

let exists = fun values ~fn ->
  let rec loop = function
    | [] -> false
    | value :: rest -> fn value || loop rest
  in
  loop values

let contains = fun values ~value ->
  let rec loop = function
    | [] -> false
    | current :: rest -> compare current value = 0 || loop rest
  in
  loop values

let head = function
  | [] -> None
  | value :: _ -> Some value

let tail = function
  | [] -> []
  | _ :: rest -> rest

let get = fun values ~at ->
  if at < 0 then
    None
  else
    let rec loop values index =
      match values with
      | [] -> None
      | value :: rest ->
          if index = 0 then
            Some value
          else
            loop rest (index - 1)
    in
    loop values at

let get_unchecked = fun values ~at ->
  match get values ~at with
  | Some value -> value
  | None -> System_error.panic
    ("List.get_unchecked received an out-of-bounds index: " ^ Int.to_string at)

let find = fun values ~fn ->
  let rec loop = function
    | [] -> None
    | value :: rest ->
        if fn value then
          Some value
        else
          loop rest
  in
  loop values

let filter = fun values ~fn ->
  let rec loop acc = function
    | [] -> reverse acc
    | value :: rest ->
        if fn value then
          loop (value :: acc) rest
        else
          loop acc rest
  in
  loop [] values

let sort = fun values ~compare ->
  let rec insert value = function
    | [] -> [ value ]
    | current :: rest as values ->
        if compare value current <= 0 then
          value :: values
        else
          current :: insert value rest
  in
  fold_left values ~acc:[] ~fn:(fun acc value -> insert value acc)

let unique = fun values ~compare ->
  let sorted = sort values ~compare in
  let rec loop acc = function
    | [] -> reverse acc
    | [ value ] -> reverse (value :: acc)
    | left :: ((right :: _) as rest) ->
        if compare left right = 0 then
          loop acc rest
        else
          loop (left :: acc) rest
  in
  loop [] sorted
