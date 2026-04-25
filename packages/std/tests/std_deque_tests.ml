open Std

module Deque = Collections.Deque

module Iterator = Iter.Iterator

module MutIterator = Iter.MutIterator

type 'a box = { mutable value: 'a }

let box = fun value -> { value }

let test_create_empty = fun _ctx ->
  let deque = Deque.create () in
  if Deque.is_empty deque && Int.equal (Deque.length deque) 0 then
    Ok ()
  else Error "expected Deque.create to start empty"

let test_with_capacity = fun _ctx ->
  let deque = Deque.with_capacity ~size:1 in
  if Deque.is_empty deque && Deque.capacity deque >= 1 then
    Ok ()
  else Error "expected Deque.with_capacity to allocate usable capacity"

let test_push_front_on_empty = fun _ctx ->
  let deque = Deque.create () in
  Deque.push_front deque ~value:7;
  if Deque.front deque = Some 7 && Deque.back deque = Some 7 then
    Ok ()
  else Error "expected push_front on empty deque to set front and back"

let test_push_back_on_empty = fun _ctx ->
  let deque = Deque.create () in
  Deque.push_back deque ~value:7;
  if Deque.front deque = Some 7 && Deque.back deque = Some 7 then
    Ok ()
  else Error "expected push_back on empty deque to set front and back"

let test_push_front_then_pop_front = fun _ctx ->
  let deque = Deque.create () in
  Deque.push_front deque ~value:7;
  if Deque.pop_front deque = Some 7 then
    Ok ()
  else Error "expected push_front/pop_front roundtrip"

let test_push_back_then_pop_back = fun _ctx ->
  let deque = Deque.create () in
  Deque.push_back deque ~value:7;
  if Deque.pop_back deque = Some 7 then
    Ok ()
  else Error "expected push_back/pop_back roundtrip"

let test_push_mix_updates_ends = fun _ctx ->
  let deque = Deque.create () in
  Deque.push_front deque ~value:2;
  Deque.push_back deque ~value:3;
  Deque.push_front deque ~value:1;
  if Deque.front deque = Some 1 && Deque.back deque = Some 3 then
    Ok ()
  else Error "expected mixed pushes to preserve both ends"

let test_pop_front_empty = fun _ctx ->
  if Deque.pop_front (Deque.create ()) = None then
    Ok ()
  else Error "expected pop_front empty = None"

let test_pop_back_empty = fun _ctx ->
  if Deque.pop_back (Deque.create ()) = None then
    Ok ()
  else Error "expected pop_back empty = None"

let test_insert_at_zero = fun _ctx ->
  let deque = Deque.from_list [ 2; 3 ] in
  Deque.insert deque ~at:0 ~value:1;
  if Deque.to_list deque = [ 1; 2; 3 ] then
    Ok ()
  else Error "expected insert at 0 to match push_front"

let test_insert_at_length = fun _ctx ->
  let deque = Deque.from_list [ 1; 2 ] in
  Deque.insert deque ~at:2 ~value:3;
  if Deque.to_list deque = [ 1; 2; 3 ] then
    Ok ()
  else Error "expected insert at length to match push_back"

let test_insert_middle = fun _ctx ->
  let deque = Deque.from_list [ 1; 3; 4 ] in
  Deque.insert deque ~at:1 ~value:2;
  if Deque.to_list deque = [
    1;
    2;
    3;
    4;
  ] then
    Ok ()
  else Error "expected insert in middle to preserve order"

let test_remove_out_of_bounds = fun _ctx ->
  if Deque.remove (Deque.from_list [ 1; 2 ]) ~at:4 = None then
    Ok ()
  else Error "expected remove out of bounds = None"

let test_remove_middle = fun _ctx ->
  let deque = Deque.from_list [ 1; 2; 3 ] in
  let removed = Deque.remove deque ~at:1 in
  if removed = Some 2 && Int.equal (Deque.length deque) 2 && Deque.to_list deque = [ 1; 3 ] then
    Ok ()
  else
    Error (
      "expected remove middle to return Some 2 and preserve [1; 3], got removed=" ^ (
        match removed with
        | None -> "None"
        | Some value -> Int.to_string value
      ) ^ " length=" ^ Int.to_string (Deque.length deque) ^ " front=" ^ (
        match Deque.front deque with
        | None -> "None"
        | Some value -> Int.to_string value
      ) ^ " back=" ^ (
        match Deque.back deque with
        | None -> "None"
        | Some value -> Int.to_string value
      )
    )

let wraparound_deque = fun () ->
  let deque = Deque.with_capacity ~size:4 in
  Deque.push_back deque ~value:1;
  Deque.push_back deque ~value:2;
  Deque.push_back deque ~value:3;
  ignore (Deque.pop_front deque);
  ignore (Deque.pop_front deque);
  Deque.push_back deque ~value:4;
  Deque.push_back deque ~value:5;
  deque

let test_get_after_wraparound = fun _ctx ->
  let deque = wraparound_deque () in
  if Deque.get deque ~at:0 = Some 3 && Deque.get deque ~at:2 = Some 5 then
    Ok ()
  else Error "expected get to honor logical indices after wrap-around"

let test_length_and_is_empty = fun _ctx ->
  let deque = Deque.create () in
  Deque.push_back deque ~value:1;
  ignore (Deque.pop_front deque);
  if Deque.is_empty deque && Int.equal (Deque.length deque) 0 then
    Ok ()
  else Error "expected length/is_empty to track mixed operations"

let test_clear = fun _ctx ->
  let deque = Deque.from_list [ 1; 2; 3 ] in
  Deque.clear deque;
  if Deque.is_empty deque && Deque.to_list deque = [] then
    Ok ()
  else Error "expected clear to empty deque"

let test_capacity_grows = fun _ctx ->
  let deque = Deque.with_capacity ~size:1 in
  let initial = Deque.capacity deque in
  Deque.push_back deque ~value:1;
  Deque.push_back deque ~value:2;
  if Deque.capacity deque > initial && Deque.to_list deque = [ 1; 2 ] then
    Ok ()
  else Error "expected capacity to grow while preserving contents"

let test_for_each_wraparound = fun _ctx ->
  let deque = wraparound_deque () in
  let visited = box [] in
  Deque.for_each deque ~fn:(
    fun value -> visited.value <- value :: visited.value
  );
  if List.reverse visited.value = [ 3; 4; 5 ] then
    Ok ()
  else Error "expected for_each to preserve logical order after wrap-around"

let test_fold_left_wraparound = fun _ctx ->
  let deque = wraparound_deque () in
  if Int.equal (Deque.fold_left deque ~init:0 ~fn:(
    fun acc value -> acc + value
  )) 12 then
    Ok ()
  else Error "expected fold_left after wrap-around to preserve logical order"

let test_to_list_mixed_pushes = fun _ctx ->
  let deque = Deque.create () in
  Deque.push_front deque ~value:2;
  Deque.push_front deque ~value:1;
  Deque.push_back deque ~value:3;
  if Deque.to_list deque = [ 1; 2; 3 ] then
    Ok ()
  else Error "expected to_list to reflect logical deque order"

let test_contains = fun _ctx ->
  let deque = Deque.from_list [ 1; 2; 3 ] in
  if Deque.contains deque ~value:2 && not (Deque.contains deque ~value:9) then
    Ok ()
  else Error "expected contains to reflect membership"

let test_append = fun _ctx ->
  let left = Deque.from_list [ 1; 2 ] in
  let right = Deque.from_list [ 3; 4 ] in
  Deque.append left right;
  if Deque.to_list left = [
    1;
    2;
    3;
    4;
  ] && Deque.is_empty right then
    Ok ()
  else Error "expected append to move right values onto left and clear right"

let test_split_off = fun _ctx ->
  let deque =
    Deque.from_list
      [
        1;
        2;
        3;
        4;
      ]
  in
  let tail = Deque.split_off deque ~at:2 in
  if Deque.to_list deque = [ 1; 2 ] && Deque.to_list tail = [ 3; 4 ] then
    Ok ()
  else Error "expected split_off to divide prefix and tail"

let test_iter = fun _ctx ->
  if Iterator.to_list (Deque.iter (Deque.from_list [ 1; 2; 3 ])) = [ 1; 2; 3 ] then
    Ok ()
  else Error "expected Deque.iter to preserve order"

let test_mut_iter = fun _ctx ->
  let deque = Deque.from_list [ 1; 2; 3 ] in
  let items = MutIterator.to_list (Deque.mut_iter deque) in
  if items = [ 1; 2; 3 ] && Deque.is_empty deque then
    Ok ()
  else Error "expected Deque.mut_iter to drain items in order"

let tests = Test.[
  case "Deque.create starts empty" test_create_empty;
  case "Deque.with_capacity allocates usable space" test_with_capacity;
  case "Deque.push_front on empty sets both ends" test_push_front_on_empty;
  case "Deque.push_back on empty sets both ends" test_push_back_on_empty;
  case "Deque.push_front then pop_front roundtrips" test_push_front_then_pop_front;
  case "Deque.push_back then pop_back roundtrips" test_push_back_then_pop_back;
  case "Deque mixed pushes preserve front and back" test_push_mix_updates_ends;
  case "Deque.pop_front on empty returns None" test_pop_front_empty;
  case "Deque.pop_back on empty returns None" test_pop_back_empty;
  case "Deque.insert at 0 matches push_front" test_insert_at_zero;
  case "Deque.insert at length matches push_back" test_insert_at_length;
  case "Deque.insert in the middle preserves order" test_insert_middle;
  case "Deque.remove out of bounds returns None" test_remove_out_of_bounds;
  case "Deque.remove middle element preserves order" test_remove_middle;
  case "Deque.get uses logical indices after wrap-around" test_get_after_wraparound;
  case "Deque.length and is_empty track mixed operations" test_length_and_is_empty;
  case "Deque.clear empties the deque" test_clear;
  case "Deque.capacity grows when buffer fills" test_capacity_grows;
  case "Deque.for_each preserves logical order after wrap-around" test_for_each_wraparound;
  case "Deque.fold_left preserves logical order after wrap-around" test_fold_left_wraparound;
  case "Deque.to_list reflects logical order" test_to_list_mixed_pushes;
  case "Deque.contains reports membership" test_contains;
  case "Deque.append moves right into left and clears right" test_append;
  case "Deque.split_off keeps prefix and tail" test_split_off;
  case "Deque.iter yields items in order" test_iter;
  case "Deque.mut_iter drains items in order" test_mut_iter;
]

let main ~args = Test.Cli.main ~name:"deque" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
