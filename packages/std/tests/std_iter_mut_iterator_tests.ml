open Std

module MutIterator = Iter.MutIterator

module ListMutIter = struct
  type state = { mutable remaining: int list }

  type item = int

  let next = fun state ->
    match state.remaining with
    | [] -> None
    | head :: tail ->
        state.remaining <- tail;
        Some head

  let size = fun state -> List.length state.remaining

  let clone = fun state -> { remaining = state.remaining }
end

let int_iter = fun items -> MutIterator.make (module ListMutIter) { ListMutIter.remaining = items }

let is_even = fun value -> Int.equal (value mod 2) 0

let test_empty_returns_no_items = fun _ctx ->
  match MutIterator.next (MutIterator.empty ()) with
  | None -> Ok ()
  | Some _ -> Error "MutIterator.empty should yield no items"

let test_singleton_yields_one_item = fun _ctx ->
  let iter = MutIterator.singleton 42 in
  match MutIterator.next iter, MutIterator.next iter with
  | Some 42, None -> Ok ()
  | _ -> Error "MutIterator.singleton should yield one item exactly once"

let test_clone_preserves_remaining_items = fun _ctx ->
  let iter = int_iter [ 1; 2; 3 ] in
  let _ = MutIterator.next iter in
  let clone = MutIterator.clone iter in
  if MutIterator.to_list iter = [ 2; 3 ] && MutIterator.to_list clone = [ 2; 3 ] then
    Ok ()
  else Error "MutIterator.clone should preserve the remaining items"

let test_map_and_filter_transform_the_sequence = fun _ctx ->
  let actual =
    int_iter
      [
        1;
        2;
        3;
        4;
      ] |> MutIterator.filter ~fn:is_even |> MutIterator.map ~fn:(
      fun value -> value * 10
    ) |> MutIterator.to_list
  in
  if actual = [ 20; 40 ] then
    Ok ()
  else Error "MutIterator.map and filter should transform the sequence"

let test_filter_map_drops_nones = fun _ctx ->
  let actual =
    MutIterator.filter_map
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
      ) |> MutIterator.to_list
  in
  if actual = [ 20; 40 ] then
    Ok ()
  else Error "MutIterator.filter_map should drop None values"

let test_flat_map_flattens_inner_iterators = fun _ctx ->
  let actual = MutIterator.flat_map (int_iter [ 1; 2; 3 ]) ~fn:(
    fun value -> MutIterator.singleton (value * 10)
  ) |> MutIterator.to_list in
  if actual = [ 10; 20; 30 ] then
    Ok ()
  else Error "MutIterator.flat_map should flatten inner iterators"

let test_fold_reduce_and_count_accumulate_items = fun _ctx ->
  let folded = MutIterator.fold (int_iter [ 1; 2; 3 ]) ~init:0 ~fn:(
    fun value acc -> acc + value
  ) in
  let reduced = MutIterator.reduce (int_iter [ 1; 2; 3 ]) ~fn:(+) in
  let counted =
    MutIterator.count
      (
        int_iter
          [
            1;
            2;
            3;
            4;
          ]
      )
  in
  if Int.equal folded 6 && reduced = Some 6 && Int.equal counted 4 then
    Ok ()
  else Error "MutIterator.fold reduce and count should accumulate items"

let test_find_any_and_all_reflect_predicates = fun _ctx ->
  let found = MutIterator.find (int_iter [ 1; 3; 4 ]) ~fn:is_even in
  let any_even = MutIterator.any (int_iter [ 1; 3; 4 ]) ~fn:is_even in
  let all_even = MutIterator.all (int_iter [ 2; 4; 6 ]) ~fn:is_even in
  if found = Some 4 && any_even && all_even then
    Ok ()
  else Error "MutIterator.find/any/all should reflect predicate matches"

let test_take_and_drop_trim_the_sequence = fun _ctx ->
  let taken =
    MutIterator.take
      (
        int_iter
          [
            1;
            2;
            3;
            4;
          ]
      )
      2 |> MutIterator.to_list
  in
  let dropped =
    MutIterator.drop
      (
        int_iter
          [
            1;
            2;
            3;
            4;
          ]
      )
      2 |> MutIterator.to_list
  in
  if taken = [ 1; 2 ] && dropped = [ 3; 4 ] then
    Ok ()
  else Error "MutIterator.take and drop should trim the sequence"

let test_enumerate_and_zip_produce_expected_pairs = fun _ctx ->
  let enumerated = MutIterator.enumerate (int_iter [ 10; 20 ]) |> MutIterator.to_list in
  let zipped = MutIterator.zip (int_iter [ 1; 2; 3 ]) (int_iter [ 4; 5 ]) |> MutIterator.to_list in
  if enumerated = [
    0, 10;
    1, 20;
  ] && zipped = [
    1, 4;
    2, 5;
  ] then
    Ok ()
  else Error "MutIterator.enumerate and zip should produce expected pairs"

let test_chain_and_for_each_preserve_order = fun _ctx ->
  let chained = MutIterator.chain (int_iter [ 1; 2 ]) (int_iter [ 3; 4 ]) in
  let seen = Sync.Atomic.make [] in
  MutIterator.for_each chained ~fn:(
    fun value -> Sync.Atomic.set seen (value :: Sync.Atomic.get seen)
  );
  if List.reverse (Sync.Atomic.get seen) = [
    1;
    2;
    3;
    4;
  ] then
    Ok ()
  else Error "MutIterator.chain and for_each should preserve order"

let tests = Test.[
  case "empty yields no items" test_empty_returns_no_items;
  case "singleton yields one item" test_singleton_yields_one_item;
  case "clone preserves remaining items" test_clone_preserves_remaining_items;
  case "map and filter transform the sequence" test_map_and_filter_transform_the_sequence;
  case "filter_map drops None values" test_filter_map_drops_nones;
  case "flat_map flattens inner iterators" test_flat_map_flattens_inner_iterators;
  case "fold reduce and count accumulate items" test_fold_reduce_and_count_accumulate_items;
  case "find any and all reflect predicates" test_find_any_and_all_reflect_predicates;
  case "take and drop trim the sequence" test_take_and_drop_trim_the_sequence;
  case "enumerate and zip produce expected pairs" test_enumerate_and_zip_produce_expected_pairs;
  case "chain and for_each preserve order" test_chain_and_for_each_preserve_order;
]

let main ~args = Test.Cli.main ~name:"iter_mut_iterator" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
