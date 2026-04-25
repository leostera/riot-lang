open Std

module Iterator = Iter.Iterator

module ListIter = struct
  type state = int list

  type item = int

  let next = function
    | [] -> None, []
    | head :: tail -> Some head, tail

  let size = List.length
end

let int_iter = fun items -> Iterator.make (module ListIter) items

let is_even = fun value -> Int.equal (value mod 2) 0

let test_to_list_collects_all_items = fun _ctx ->
  if Iterator.to_list (int_iter [ 1; 2; 3 ]) = [ 1; 2; 3 ] then
    Ok ()
  else Error "Iterator.to_list should collect every item in order"

let test_map_transforms_each_item = fun _ctx ->
  if Iterator.to_list (Iterator.map (int_iter [ 1; 2; 3 ]) ~fn:(
    fun value -> value * 2
  )) = [ 2; 4; 6 ] then
    Ok ()
  else Error "Iterator.map should transform every item"

let test_filter_keeps_only_matching_items = fun _ctx ->
  if Iterator.to_list
    (
      Iterator.filter
        (
          int_iter
            [
              1;
              2;
              3;
              4;
            ]
        )
        ~fn:is_even
    ) = [ 2; 4 ] then
    Ok ()
  else Error "Iterator.filter should keep only matching items"

let test_filter_map_drops_nones_and_unwraps_somes = fun _ctx ->
  let actual =
    Iterator.filter_map
      (
        int_iter
          [
            1;
            2;
            3;
            4;
          ]
      )
      ~fn:(
        fun value ->
          if is_even value then
            Some (value * 10)
          else None
      ) |> Iterator.to_list
  in
  if actual = [ 20; 40 ] then
    Ok ()
  else Error "Iterator.filter_map should drop None values and unwrap Some values"

let test_fold_and_reduce_accumulate_items = fun _ctx ->
  let folded = Iterator.fold (int_iter [ 1; 2; 3 ]) ~init:0 ~fn:(
    fun value acc -> acc + value
  ) in
  let reduced = Iterator.reduce (int_iter [ 1; 2; 3 ]) ~fn:(+) in
  if Int.equal folded 6 && reduced = Some 6 then
    Ok ()
  else Error "Iterator.fold and Iterator.reduce should accumulate items"

let test_count_returns_number_of_items = fun _ctx ->
  if Int.equal
    (
      Iterator.count
        (
          int_iter
            [
              1;
              2;
              3;
              4;
            ]
        )
    )
    4 then
    Ok ()
  else Error "Iterator.count should return the number of items"

let test_find_any_and_all_reflect_predicates = fun _ctx ->
  let found = Iterator.find (int_iter [ 1; 3; 4 ]) ~fn:is_even in
  let any_even = Iterator.any (int_iter [ 1; 3; 4 ]) ~fn:is_even in
  let all_even = Iterator.all (int_iter [ 2; 4; 6 ]) ~fn:is_even in
  if found = Some 4 && any_even && all_even then
    Ok ()
  else Error "Iterator.find/any/all should reflect predicate matches"

let test_take_and_drop_trim_the_sequence = fun _ctx ->
  let taken =
    Iterator.take
      (
        int_iter
          [
            1;
            2;
            3;
            4;
          ]
      )
      2 |> Iterator.to_list
  in
  let dropped =
    Iterator.drop
      (
        int_iter
          [
            1;
            2;
            3;
            4;
          ]
      )
      2 |> Iterator.to_list
  in
  if taken = [ 1; 2 ] && dropped = [ 3; 4 ] then
    Ok ()
  else Error "Iterator.take and Iterator.drop should trim the sequence"

let test_enumerate_pairs_indices_with_values = fun _ctx ->
  let actual = Iterator.enumerate (int_iter [ 10; 20 ]) |> Iterator.to_list in
  if actual = [
    0, 10;
    1, 20;
  ] then
    Ok ()
  else Error "Iterator.enumerate should pair indices with values"

let test_zip_stops_at_the_shorter_input = fun _ctx ->
  let actual = Iterator.zip (int_iter [ 1; 2; 3 ]) (int_iter [ 4; 5 ]) |> Iterator.to_list in
  if actual = [
    1, 4;
    2, 5;
  ] then
    Ok ()
  else Error "Iterator.zip should stop when either input is exhausted"

let test_chain_appends_the_second_iterator = fun _ctx ->
  let actual = Iterator.chain (int_iter [ 1; 2 ]) (int_iter [ 3; 4 ]) |> Iterator.to_list in
  if actual = [
    1;
    2;
    3;
    4;
  ] then
    Ok ()
  else Error "Iterator.chain should append the second iterator after the first"

let test_for_each_visits_items_in_order = fun _ctx ->
  let seen = Sync.Atomic.make [] in
  Iterator.for_each (int_iter [ 1; 2; 3 ]) ~fn:(
    fun value -> Sync.Atomic.set seen (value :: Sync.Atomic.get seen)
  );
  if List.reverse (Sync.Atomic.get seen) = [ 1; 2; 3 ] then
    Ok ()
  else Error "Iterator.for_each should visit items in order"

let tests = Test.[
  case "to_list collects all items" test_to_list_collects_all_items;
  case "map transforms each item" test_map_transforms_each_item;
  case "filter keeps only matching items" test_filter_keeps_only_matching_items;
  case "filter_map drops None and unwraps Some" test_filter_map_drops_nones_and_unwraps_somes;
  case "fold and reduce accumulate items" test_fold_and_reduce_accumulate_items;
  case "count returns the number of items" test_count_returns_number_of_items;
  case "find any and all reflect predicates" test_find_any_and_all_reflect_predicates;
  case "take and drop trim the sequence" test_take_and_drop_trim_the_sequence;
  case "enumerate pairs indices with values" test_enumerate_pairs_indices_with_values;
  case "zip stops at the shorter input" test_zip_stops_at_the_shorter_input;
  case "chain appends the second iterator" test_chain_appends_the_second_iterator;
  case "for_each visits items in order" test_for_each_visits_items_in_order;
]

let main ~args = Test.Cli.main ~name:"iter_iterator" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
