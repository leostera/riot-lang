open Kernel

type 'value t = ('value, unit) Hashmap.t

let create = Hashmap.create

let with_capacity = Hashmap.with_capacity

let from_list = fun values ->
  let set = with_capacity ~size:(List.length values) in
  List.for_each values
    ~fn:(fun value ->
      let _ = Hashmap.insert set ~key:value ~value:() in
      ());
  set

let insert = fun set ~value ->
  match Hashmap.insert set ~key:value ~value:() with
  | Some () -> false
  | None -> true

let remove = fun set ~value ->
  match Hashmap.remove set ~key:value with
  | Some () -> true
  | None -> false

let contains = fun set ~value -> Hashmap.has_key set ~key:value

let length = Hashmap.length

let is_empty = Hashmap.is_empty

let clear = Hashmap.clear

let for_each = fun set ~fn -> Hashmap.for_each set ~fn:(fun value () -> fn value)

let fold_left = fun set ~acc ~fn -> Hashmap.fold_left set ~acc ~fn:(fun acc value () -> fn acc value)

let to_list = fun set -> Hashmap.keys set

let union = fun left right ->
  let result = from_list (to_list left) in
  for_each right
    ~fn:(fun value ->
      let _ = Hashmap.insert result ~key:value ~value:() in
      ());
  result

let intersection = fun left right ->
  let result = create () in
  for_each left
    ~fn:(fun value ->
      if contains right ~value then
        let _ = Hashmap.insert result ~key:value ~value:() in
        ());
  result

let difference = fun left right ->
  let result = create () in
  for_each left
    ~fn:(fun value ->
      if not (contains right ~value) then
        let _ = Hashmap.insert result ~key:value ~value:() in
        ());
  result

let symmetric_difference = fun left right ->
  let result = create () in
  for_each left
    ~fn:(fun value ->
      if not (contains right ~value) then
        let _ = Hashmap.insert result ~key:value ~value:() in
        ());
  for_each right
    ~fn:(fun value ->
      if not (contains left ~value) then
        let _ = Hashmap.insert result ~key:value ~value:() in
        ());
  result

let is_subset = fun left right ->
  fold_left left ~acc:true ~fn:(fun acc value -> acc && contains right ~value)

let is_superset = fun left right -> is_subset right left

let is_disjoint = fun left right ->
  fold_left left ~acc:true ~fn:(fun acc value -> acc && not (contains right ~value))

let iter: type value. value t -> value Iter.Iterator.t = fun set ->
  let module SetIter = struct
    type state = value list

    type item = value

    let next = fun state ->
      match state with
      | [] -> (None, [])
      | value :: rest -> (Some value, rest)

    let size = fun state -> List.length state
  end in
  Iter.Iterator.make (module SetIter) (to_list set)

let mut_iter: type value. value t -> value Iter.MutIterator.t = fun set ->
  let module SetIter = struct
    type state = {
      mutable items: value list;
    }

    type item = value

    let next = fun state ->
      match state.items with
      | [] -> None
      | value :: rest ->
          state.items <- rest;
          Some value

    let size = fun state -> List.length state.items

    let clone = fun state -> { items = state.items }
  end in
  Iter.MutIterator.make (module SetIter) { items = to_list set }
