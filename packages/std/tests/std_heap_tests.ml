open Std
module Heap = Collections.Heap
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

type 'a box = {
  mutable value: 'a;
}

let box = fun value -> { value }

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let test_create = fun _ctx ->
  let heap = Heap.create () in
  if Heap.is_empty heap then
    Ok ()
  else
    Error "expected Heap.create to start empty"

let test_create_max = fun _ctx ->
  let heap = Heap.create_max () in
  List.for_each [ 1; 3; 2 ] ~fn:(fun value -> Heap.push heap ~value);
  if Heap.peek heap = Some 3 then
    Ok ()
  else
    Error "expected max-heap peek to return maximum"

let test_create_with_custom_compare = fun _ctx ->
  let heap =
    Heap.create_with
      ~compare:(fun left right ->
        Int.compare (left mod 10) (right mod 10))
      ()
  in
  List.for_each [ 32; 25; 18 ] ~fn:(fun value -> Heap.push heap ~value);
  if Heap.peek heap = Some 32 then
    Ok ()
  else
    Error "expected custom compare to determine heap root"

let test_from_list = fun _ctx ->
  if Heap.peek (Heap.from_list [ 3; 1; 2 ]) = Some 1 then
    Ok ()
  else
    Error "expected from_list min-heap peek to return minimum"

let test_from_list_with = fun _ctx ->
  if Heap.peek
      (
        Heap.from_list_with
          ~compare:(fun left right ->
            Int.compare right left)
          [ 3; 1; 2 ]
      ) = Some 3 then
    Ok ()
  else
    Error "expected from_list_with custom ordering to determine root"

let test_push_updates_peek = fun _ctx ->
  let heap = Heap.create () in
  Heap.push heap ~value:3;
  Heap.push heap ~value:1;
  if Heap.peek heap = Some 1 then
    Ok ()
  else
    Error "expected push to update peek to smallest element"

let test_pop_empty = fun _ctx ->
  if Heap.pop (Heap.create ()) = None then
    Ok ()
  else
    Error "expected pop empty = None"

let test_pop_repeated = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  let popped = [ Heap.pop heap; Heap.pop heap; Heap.pop heap ]
  |> List.filter_map ~fn:(fun value -> value) in
  if sort_ints popped = [ 1; 2; 3 ] && Heap.is_empty heap then
    Ok ()
  else
    Error "expected repeated pops to return ascending order"

let test_pop_unchecked = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  if Int.equal (Heap.pop_unchecked heap) 1 then
    Ok ()
  else
    Error "expected pop_unchecked to return root of non-empty heap"

let test_peek_empty = fun _ctx ->
  if Heap.peek (Heap.create ()) = None then
    Ok ()
  else
    Error "expected peek empty = None"

let test_peek_unchecked = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  if Int.equal (Heap.peek_unchecked heap) 1 then
    Ok ()
  else
    Error "expected peek_unchecked to return root of non-empty heap"

let test_length = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  if Int.equal (Heap.length heap) 3 then
    Ok ()
  else
    Error "expected Heap.length to track live items"

let test_is_empty_after_removing_all = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  ignore (Heap.pop heap);
  ignore (Heap.pop heap);
  ignore (Heap.pop heap);
  if Heap.is_empty heap then
    Ok ()
  else
    Error "expected heap to be empty after removing all items"

let test_clear = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  Heap.clear heap;
  if Heap.is_empty heap then
    Ok ()
  else
    Error "expected clear to empty heap"

let test_to_list = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  if Heap.to_list heap = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected to_list to return ascending list and consume heap"

let test_to_list_unordered = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  if sort_ints (Heap.to_list_unordered heap) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected to_list_unordered to preserve heap multiset"

let test_for_each = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  let seen = box [] in
  Heap.for_each heap ~fn:(fun value -> seen.value <- value :: seen.value);
  if List.reverse seen.value = [ 1; 2; 3 ] && Heap.is_empty heap then
    Ok ()
  else
    Error "expected for_each to visit each item once in heap order"

let test_fold_left = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  if
    String.equal (Heap.fold_left heap ~init:"" ~fn:(fun acc value -> acc ^ Int.to_string value)) "123"
  then
    Ok ()
  else
    Error "expected fold_left to consume heap in heap order"

let test_iter = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  let items = Iterator.to_list (Heap.iter heap) in
  if items = [ 1; 2; 3 ] && Heap.peek heap = Some 1 then
    Ok ()
  else
    Error "expected iter to preserve original heap"

let test_mut_iter = fun _ctx ->
  let heap = Heap.from_list [ 3; 1; 2 ] in
  let items = MutIterator.to_list (Heap.mut_iter heap) in
  if items = [ 1; 2; 3 ] && Heap.is_empty heap then
    Ok ()
  else
    Error "expected mut_iter to drain heap in order"

let tests =
  Test.[
    case "Heap.create starts empty" test_create;
    case "Heap.create_max returns maximum on peek" test_create_max;
    case "Heap.create_with respects custom compare" test_create_with_custom_compare;
    case "Heap.from_list uses min-heap ordering" test_from_list;
    case "Heap.from_list_with respects custom compare" test_from_list_with;
    case "Heap.push updates peek to minimum" test_push_updates_peek;
    case "Heap.pop on empty returns None" test_pop_empty;
    case "Heap.pop returns ascending values on repeated pops" test_pop_repeated;
    case "Heap.pop_unchecked returns root on non-empty heap" test_pop_unchecked;
    case "Heap.peek on empty returns None" test_peek_empty;
    case "Heap.peek_unchecked returns root on non-empty heap" test_peek_unchecked;
    case "Heap.length tracks live items" test_length;
    case "Heap.is_empty after removing all items" test_is_empty_after_removing_all;
    case "Heap.clear empties the heap" test_clear;
    case "Heap.to_list returns ascending values" test_to_list;
    case "Heap.to_list_unordered preserves the multiset" test_to_list_unordered;
    case "Heap.for_each visits each item once" test_for_each;
    case "Heap.fold_left consumes values in heap order" test_fold_left;
    case "Heap.iter does not mutate the heap" test_iter;
    case "Heap.mut_iter drains the heap" test_mut_iter;
  ]

let main ~args = Test.Cli.main ~name:"heap" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
