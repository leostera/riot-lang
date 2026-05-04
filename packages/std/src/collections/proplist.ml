open Kernel

type ('key, 'value) t = ('key * 'value) list

let empty = []

let is_empty = fun __tmp1 ->
  match __tmp1 with
  | [] -> true
  | _ -> false

let length = List.length

let from_list entries = entries

let to_list entries = entries

let get entries ~key =
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | (entry_key, value) :: _ when entry_key = key -> Some value
    | _ :: rest -> loop rest
  in
  loop entries

let get_all entries ~key =
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | (entry_key, value) :: rest when entry_key = key -> loop (value :: acc) rest
    | _ :: rest -> loop acc rest
  in
  loop [] entries

let contains_key entries ~key = Option.is_some (get entries ~key)

let add entries ~key ~value = (key, value) :: entries

let set entries ~key ~value =
  let rec loop acc found = fun __tmp1 ->
    match __tmp1 with
    | [] ->
        if found then
          List.reverse acc
        else
          List.reverse ((key, value) :: acc)
    | (entry_key, _) :: rest when entry_key = key ->
        if found then
          loop acc found rest
        else
          loop ((key, value) :: acc) true rest
    | entry :: rest -> loop (entry :: acc) found rest
  in
  loop [] false entries

let remove entries ~key =
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | ((entry_key, _) as entry) :: rest ->
        if entry_key = key then
          loop acc rest
        else
          loop (entry :: acc) rest
  in
  loop [] entries

let keys entries =
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | (key, _) :: rest -> loop (key :: acc) rest
  in
  loop [] entries

let values entries =
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | (_, value) :: rest -> loop (value :: acc) rest
  in
  loop [] entries

let for_each entries ~fn =
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | (key, value) :: rest ->
        fn key value;
        loop rest
  in
  loop entries

let fold_left entries ~init ~fn =
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> acc
    | (key, value) :: rest -> loop (fn acc key value) rest
  in
  loop init entries

let iter: type key value. (key, value) t -> (key * value) Iter.Iterator.t = fun entries ->
  let module ProplistIter = struct
    type state = (key * value) list

    type item = key * value

    let next = fun __tmp1 ->
      match __tmp1 with
      | [] -> (None, [])
      | entry :: rest -> (Some entry, rest)

    let size = List.length
  end in
  Iter.Iterator.make (module ProplistIter) entries
