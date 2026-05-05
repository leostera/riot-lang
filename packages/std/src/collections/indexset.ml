open Kernel

type 'value t = {
  items: 'value Vector.t;
  seen: 'value Hashset.t;
}

let create = fun () -> { items = Vector.create (); seen = Hashset.create () }

let with_capacity = fun ~size -> {
  items = Vector.with_capacity ~size;
  seen = Hashset.with_capacity ~size;
}

let insert = fun set ~value ->
  if Hashset.insert set.seen ~value then (
    Vector.push set.items ~value;
    true
  ) else
    false

let from_list = fun values ->
  let set = with_capacity ~size:(List.length values) in
  List.for_each
    values
    ~fn:(fun value ->
      let _ = insert set ~value in
      ());
  set

let remove = fun set ~value ->
  if Hashset.remove set.seen ~value then (
    let next_items = Vector.with_capacity ~size:(Vector.length set.items) in
    Vector.for_each
      set.items
      ~fn:(fun item ->
        if not (item = value) then
          Vector.push next_items ~value:item);
    Vector.clear set.items;
    Vector.append set.items next_items;
    true
  ) else
    false

let contains = fun set ~value -> Hashset.contains set.seen ~value

let length = fun set -> Vector.length set.items

let is_empty = fun set -> Vector.is_empty set.items

let clear = fun set ->
  Vector.clear set.items;
  Hashset.clear set.seen

let for_each = fun set ~fn -> Vector.for_each set.items ~fn

let fold_left = fun set ~init ~fn ->
  let rec loop index acc =
    if Int.equal index (Vector.length set.items) then
      acc
    else
      loop (Int.add index 1) (fn acc (Vector.get_unchecked set.items ~at:index))
  in
  loop 0 init

let to_list = fun set ->
  fold_left set ~init:[] ~fn:(fun acc value -> value :: acc)
  |> List.reverse

let union = fun left right ->
  let result = from_list (to_list left) in
  for_each
    right
    ~fn:(fun value ->
      let _ = insert result ~value in
      ());
  result

let intersection = fun left right ->
  let result = with_capacity ~size:(Int.min (length left) (length right)) in
  for_each
    left
    ~fn:(fun value ->
      if contains right ~value then
        let _ = insert result ~value in
        ());
  result

let difference = fun left right ->
  let result = with_capacity ~size:(length left) in
  for_each
    left
    ~fn:(fun value ->
      if not (contains right ~value) then
        let _ = insert result ~value in
        ());
  result

let symmetric_difference = fun left right ->
  let result = with_capacity ~size:(length left + length right) in
  for_each
    left
    ~fn:(fun value ->
      if not (contains right ~value) then
        let _ = insert result ~value in
        ());
  for_each
    right
    ~fn:(fun value ->
      if not (contains left ~value) then
        let _ = insert result ~value in
        ());
  result

let is_subset = fun left right ->
  fold_left
    left
    ~init:true
    ~fn:(fun acc value -> acc && contains right ~value)

let is_superset = fun left right -> is_subset right left

let is_disjoint = fun left right ->
  fold_left
    left
    ~init:true
    ~fn:(fun acc value -> acc && not (contains right ~value))

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
