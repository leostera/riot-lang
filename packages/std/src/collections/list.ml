open Kernel

type 'value t = 'value list

let length = Kernel.List.length

let compare_lengths = fun ~left ~right ->
  let rec loop left_values right_values =
    match (left_values, right_values) with
    | [], [] -> 0
    | [], _ -> (-1)
    | _, [] -> 1
    | _ :: next_left, _ :: next_right -> loop next_left next_right
  in
  loop left right

let is_empty = Kernel.List.is_empty

let append = Kernel.List.append

let reverse = Kernel.List.reverse

let rec reverse_append = fun left right ->
  match left with
  | [] -> right
  | value :: rest -> reverse_append rest (value :: right)

let concat = Kernel.List.concat

let init = fun ~count ~fn -> Kernel.List.init ~count ~fn

let head = Kernel.List.head

let tail = Kernel.List.tail

let get = fun values ~at -> Kernel.List.get values ~at

let get_unchecked = fun values ~at -> Kernel.List.get_unchecked values ~at

let enumerate = fun list ->
  let rec loop list idx =
    match list with
    | [] -> []
    | x :: xs -> (idx, x) :: (loop xs (idx + 1))
  in
  loop list 0

let map = fun values ~fn -> Kernel.List.map values ~fn

let flat_map = fun values ~fn ->
  let rec loop acc = function
    | [] -> acc
    | value :: rest -> loop (fn value @ acc) rest
  in
  loop [] values

let for_each = fun values ~fn -> Kernel.List.for_each values ~fn

let fold_left = fun values ~acc ~fn -> Kernel.List.fold_left values ~acc ~fn

let fold_right = fun values ~acc ~fn -> Kernel.List.fold_right values ~acc ~fn

let all = fun values ~fn ->
  let rec loop values =
    match values with
    | [] -> true
    | value :: rest -> fn value && loop rest
  in
  loop values

let any = fun values ~fn ->
  let rec loop values =
    match values with
    | [] -> false
    | value :: rest -> fn value || loop rest
  in
  loop values

let contains = fun values ~value -> Kernel.List.contains values ~value

let find = fun values ~fn -> Kernel.List.find values ~fn

let filter = fun values ~fn -> Kernel.List.filter values ~fn

let filter_map = fun values ~fn ->
  let rec loop acc = function
    | [] -> reverse acc
    | value :: rest -> (
        match fn value with
        | Some mapped -> loop (mapped :: acc) rest
        | None -> loop acc rest
      )
  in
  loop [] values

let sort = fun values ~compare -> Kernel.List.sort values ~compare

let unique = fun values ~compare -> Kernel.List.unique values ~compare

let zip = fun left right ->
  let rec loop acc left_values right_values =
    match (left_values, right_values) with
    | [], [] -> reverse acc
    | left_value :: left_rest, right_value :: right_rest -> loop
      ((left_value, right_value) :: acc)
      left_rest
      right_rest
    | _ -> Kernel.SystemError.panic "List.zip received lists with different lengths"
  in
  loop [] left right

let unzip = fun values ->
  let rec loop left_acc right_acc values =
    match values with
    | [] -> (reverse left_acc, reverse right_acc)
    | (left, right) :: rest -> loop (left :: left_acc) (right :: right_acc) rest
  in
  loop [] [] values
