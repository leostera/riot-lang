open Prelude

type 'value t = 'value list

let length =
  let rec loop count = fun __tmp1 ->
    match __tmp1 with
    | [] -> count
    | _ :: rest -> loop (count + 1) rest
  in
  fun values -> loop 0 values

let is_empty = fun __tmp1 ->
  match __tmp1 with
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
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> reverse acc
    | values :: rest -> loop (reverse_append values acc) rest
  in
  fun values -> loop [] values

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
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> reverse acc
    | value :: rest -> loop (fn value :: acc) rest
  in
  loop [] values

let for_each = fun values ~fn ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | value :: rest ->
        fn value;
        loop rest
  in
  loop values

let fold_left = fun values ~acc ~fn ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
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
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> false
    | value :: rest -> fn value || loop rest
  in
  loop values

let contains = fun values ~value ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> false
    | current :: rest ->
        match compare current value with
        | Order.EQ -> true
        | Order.LT
        | Order.GT -> loop rest
  in
  loop values

let head = fun __tmp1 ->
  match __tmp1 with
  | [] -> None
  | value :: _ -> Some value

let tail = fun __tmp1 ->
  match __tmp1 with
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
  | None ->
      System_error.panic ("List.get_unchecked received an out-of-bounds index: " ^ Int.to_string at)

let find = fun values ~fn ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | value :: rest ->
        if fn value then
          Some value
        else
          loop rest
  in
  loop values

let filter = fun values ~fn ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> reverse acc
    | value :: rest ->
        if fn value then
          loop (value :: acc) rest
        else
          loop acc rest
  in
  loop [] values

let sort = fun values ~compare ->
  let rec insert value = fun __tmp1 ->
    match __tmp1 with
    | [] -> [ value ]
    | current :: rest as values ->
        match compare value current with
        | Order.LT
        | Order.EQ -> value :: values
        | Order.GT -> current :: insert value rest
  in
  fold_left values ~acc:[] ~fn:(fun acc value -> insert value acc)

let unique = fun values ~compare ->
  let sorted = sort values ~compare in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> reverse acc
    | [ value ] -> reverse (value :: acc)
    | left :: ((right :: _) as rest) ->
        match compare left right with
        | Order.EQ -> loop acc rest
        | Order.LT
        | Order.GT -> loop (left :: acc) rest
  in
  loop [] sorted
