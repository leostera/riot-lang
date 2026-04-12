open Prelude

type 'value t = 'value list

let rec length_aux count = function
  | [] -> count
  | _ :: rest -> length_aux (count + 1) rest

let length = fun values -> length_aux 0 values

let rec compare_lengths left right =
  match (left, right) with
  | [], [] -> 0
  | [], _ -> (-1)
  | _, [] -> 1
  | _ :: left, _ :: right -> compare_lengths left right

let is_empty = function
  | [] -> true
  | _ -> false

let hd = function
  | [] -> raise (Failure "hd")
  | value :: _ -> value

let tl = function
  | [] -> raise (Failure "tl")
  | _ :: rest -> rest

let nth = fun values index ->
  if index < 0 then
    raise (Invalid_argument "List.nth")
  else
    let rec nth_aux values index =
      match values with
      | [] -> raise (Failure "nth")
      | value :: rest ->
          if index = 0 then
            value
          else
            nth_aux rest (index - 1)
    in
    nth_aux values index

let rec append left right =
  match left with
  | [] -> right
  | value :: rest -> value :: append rest right

let rec rev_append left right =
  match left with
  | [] -> right
  | value :: rest -> rev_append rest (value :: right)

let rev = fun values -> rev_append values []

let init = fun len f ->
  if len < 0 then
    raise (Invalid_argument "List.init")
  else
    let rec build index acc =
      if index < 0 then
        acc
      else
        build (index - 1) (f index :: acc)
    in
    build (len - 1) []

let concat =
  let rec go acc = function
    | [] -> rev acc
    | values :: rest -> go (rev_append values acc) rest
  in
  fun values -> go [] (rev values)

let map = fun f values ->
  let rec go acc = function
    | [] -> rev acc
    | value :: rest -> go (f value :: acc) rest
  in
  go [] values

let mapi = fun f values ->
  let rec map_aux index acc = function
    | [] -> rev acc
    | value :: rest -> map_aux (index + 1) (f index value :: acc) rest
  in
  map_aux 0 [] values

let rev_map = fun f values ->
  let rec go acc = function
    | [] -> acc
    | value :: rest -> go (f value :: acc) rest
  in
  go [] values

let rec iter f = function
  | [] -> ()
  | value :: rest ->
      f value;
      iter f rest

let iteri = fun f values ->
  let rec go index = function
    | [] -> ()
    | value :: rest ->
        f index value;
        go (index + 1) rest
  in
  go 0 values

let rec fold_left f acc = function
  | [] -> acc
  | value :: rest -> fold_left f (f acc value) rest

let rec fold_right f values acc =
  match values with
  | [] -> acc
  | value :: rest -> f value (fold_right f rest acc)

let rec iter2 f left right =
  match (left, right) with
  | [], [] ->
      ()
  | left :: left_rest, right :: right_rest ->
      f left right;
      iter2 f left_rest right_rest
  | _ ->
      raise (Invalid_argument "List.iter2")

let map2 = fun f left right ->
  let rec go acc left right =
    match (left, right) with
    | [], [] -> rev acc
    | left :: left_rest, right :: right_rest -> go (f left right :: acc) left_rest right_rest
    | _ -> raise (Invalid_argument "List.map2")
  in
  go [] left right

let rev_map2 = fun f left right ->
  let rec go acc left right =
    match (left, right) with
    | [], [] -> acc
    | left :: left_rest, right :: right_rest -> go (f left right :: acc) left_rest right_rest
    | _ -> raise (Invalid_argument "List.rev_map2")
  in
  go [] left right

let rec fold_left2 f acc left right =
  match (left, right) with
  | [], [] -> acc
  | left :: left_rest, right :: right_rest -> fold_left2 f (f acc left right) left_rest right_rest
  | _ -> raise (Invalid_argument "List.fold_left2")

let rec fold_right2 f left right acc =
  match (left, right) with
  | [], [] -> acc
  | left :: left_rest, right :: right_rest -> f left right (fold_right2 f left_rest right_rest acc)
  | _ -> raise (Invalid_argument "List.fold_right2")

let rec for_all2 f left right =
  match (left, right) with
  | [], [] -> true
  | left :: left_rest, right :: right_rest -> f left right && for_all2 f left_rest right_rest
  | _ -> raise (Invalid_argument "List.for_all2")

let rec exists2 f left right =
  match (left, right) with
  | [], [] -> false
  | left :: left_rest, right :: right_rest -> f left right || exists2 f left_rest right_rest
  | _ -> raise (Invalid_argument "List.exists2")

let rec exists f = function
  | [] -> false
  | value :: rest -> f value || exists f rest

let rec mem target = function
  | [] -> false
  | value :: rest -> compare value target = 0 || mem target rest

let rec assoc target = function
  | [] -> raise Not_found
  | (key, value) :: rest ->
      if compare key target = 0 then
        value
      else
        assoc target rest

let rec assoc_opt target = function
  | [] -> None
  | (key, value) :: rest ->
      if compare key target = 0 then
        Some value
      else
        assoc_opt target rest

let rec remove_assoc target = function
  | [] -> []
  | (key, _ as pair) :: rest ->
      if compare key target = 0 then
        rest
      else
        pair :: remove_assoc target rest

let rec find f = function
  | [] -> raise Not_found
  | value :: rest ->
      if f value then
        value
      else
        find f rest

let rec find_opt f = function
  | [] -> None
  | value :: rest ->
      if f value then
        Some value
      else
        find_opt f rest

let rec find_map f = function
  | [] -> None
  | value :: rest -> (
      match f value with
      | Some _ as result -> result
      | None -> find_map f rest
    )

let filter = fun f values ->
  let rec go acc = function
    | [] -> rev acc
    | value :: rest ->
        if f value then
          go (value :: acc) rest
        else
          go acc rest
  in
  go [] values

let filter_map = fun f values ->
  let rec go acc = function
    | [] -> rev acc
    | value :: rest -> (
        match f value with
        | None -> go acc rest
        | Some mapped -> go (mapped :: acc) rest
      )
  in
  go [] values

let combine = fun left right ->
  let rec go acc left right =
    match (left, right) with
    | [], [] -> rev acc
    | left :: left_rest, right :: right_rest -> go ((left, right) :: acc) left_rest right_rest
    | _ -> raise (Invalid_argument "List.combine")
  in
  go [] left right

let sort = fun cmp values ->
  let rec insert value = function
    | [] -> [ value ]
    | current :: rest as values ->
        if cmp value current <= 0 then
          value :: values
        else
          current :: insert value rest
  in
  fold_left (fun acc value -> insert value acc) [] values

let sort_uniq = fun cmp values ->
  let sorted = sort cmp values in
  let rec go acc = function
    | [] -> rev acc
    | [ value ] -> rev (value :: acc)
    | left :: ((right :: _) as rest) ->
        if cmp left right = 0 then
          go acc rest
        else
          go (left :: acc) rest
  in
  go [] sorted
