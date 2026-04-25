open Std
module HashSet = Collections.HashSet
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

type 'a box = {
  mutable value: 'a;
}

let box = fun value -> { value }

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let test_create = fun _ctx ->
  let set = HashSet.create () in
  if HashSet.is_empty set && Int.equal (HashSet.length set) 0 then
    Ok ()
  else
    Error "expected HashSet.create to start empty"

let test_with_capacity = fun _ctx ->
  let set = HashSet.with_capacity ~size:16 in
  if HashSet.is_empty set then
    Ok ()
  else
    Error "expected HashSet.with_capacity to start empty"

let test_from_list_with_duplicates = fun _ctx ->
  let set = HashSet.from_list [ 1; 2; 2; 3 ] in
  if sort_ints (HashSet.to_list set) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected from_list to deduplicate members"

let test_insert_new = fun _ctx ->
  let set = HashSet.create () in
  ignore (HashSet.insert set ~value:1);
  if HashSet.contains set ~value:1 && Int.equal (HashSet.length set) 1 then
    Ok ()
  else
    Error "expected insert new value to return true and grow set"

let test_insert_duplicate = fun _ctx ->
  let set = HashSet.from_list [ 1 ] in
  if not (HashSet.insert set ~value:1) && Int.equal (HashSet.length set) 1 then
    Ok ()
  else
    Error "expected duplicate insert to return false"

let test_remove_existing = fun _ctx ->
  let set = HashSet.from_list [ 1 ] in
  ignore (HashSet.remove set ~value:1);
  if HashSet.is_empty set && not (HashSet.contains set ~value:1) then
    Ok ()
  else
    Error "expected remove existing value to return true"

let test_remove_missing = fun _ctx ->
  let set = HashSet.from_list [ 1 ] in
  if not (HashSet.remove set ~value:2) then
    Ok ()
  else
    Error "expected remove missing value to return false"

let test_contains = fun _ctx ->
  let set = HashSet.from_list [ 1; 2 ] in
  if HashSet.contains set ~value:1 && not (HashSet.contains set ~value:9) then
    Ok ()
  else
    Error "expected contains to reflect membership"

let test_length_after_duplicate_inserts = fun _ctx ->
  let set = HashSet.create () in
  ignore (HashSet.insert set ~value:1);
  ignore (HashSet.insert set ~value:1);
  if Int.equal (HashSet.length set) 1 then
    Ok ()
  else
    Error "expected length to count unique members only"

let test_clear = fun _ctx ->
  let set = HashSet.from_list [ 1; 2; 3 ] in
  HashSet.clear set;
  if HashSet.is_empty set && HashSet.to_list set = [] then
    Ok ()
  else
    Error "expected clear to empty set"

let test_for_each = fun _ctx ->
  let set = HashSet.from_list [ 1; 2; 3 ] in
  let seen = box [] in
  HashSet.for_each set ~fn:(fun value -> seen.value <- value :: seen.value);
  if sort_ints seen.value = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected for_each to visit each member exactly once"

let test_fold_left = fun _ctx ->
  let set = HashSet.from_list [ 1; 2; 3 ] in
  if Int.equal (HashSet.fold_left set ~init:0 ~fn:(fun acc value -> acc + value)) 6 then
    Ok ()
  else
    Error "expected fold_left to accumulate all members"

let test_to_list = fun _ctx ->
  let set = HashSet.from_list [ 1; 2; 3 ] in
  if sort_ints (HashSet.to_list set) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected to_list to contain each member once"

let test_union = fun _ctx ->
  let actual = HashSet.union (HashSet.from_list [ 1; 2 ]) (HashSet.from_list [ 2; 3 ]) in
  if sort_ints (HashSet.to_list actual) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected union to contain all distinct members"

let test_intersection = fun _ctx ->
  let actual = HashSet.intersection (HashSet.from_list [ 1; 2 ]) (HashSet.from_list [ 2; 3 ]) in
  if sort_ints (HashSet.to_list actual) = [ 2 ] then
    Ok ()
  else
    Error "expected intersection to contain shared members only"

let test_difference = fun _ctx ->
  let actual = HashSet.difference (HashSet.from_list [ 1; 2; 3 ]) (HashSet.from_list [ 2 ]) in
  if sort_ints (HashSet.to_list actual) = [ 1; 3 ] then
    Ok ()
  else
    Error "expected difference to contain only left-only members"

let test_symmetric_difference = fun _ctx ->
  let actual = HashSet.symmetric_difference (HashSet.from_list [ 1; 2 ]) (HashSet.from_list [ 2; 3 ]) in
  if sort_ints (HashSet.to_list actual) = [ 1; 3 ] then
    Ok ()
  else
    Error "expected symmetric difference to contain non-overlapping members"

let test_is_subset_true = fun _ctx ->
  if HashSet.is_subset (HashSet.from_list [ 1; 2 ]) (HashSet.from_list [ 1; 2; 3 ]) then
    Ok ()
  else
    Error "expected smaller set to be subset of larger set"

let test_is_subset_false = fun _ctx ->
  if not (HashSet.is_subset (HashSet.from_list [ 1; 4 ]) (HashSet.from_list [ 1; 2; 3 ])) then
    Ok ()
  else
    Error "expected non-contained set not to be subset"

let test_is_superset_true = fun _ctx ->
  if HashSet.is_superset (HashSet.from_list [ 1; 2; 3 ]) (HashSet.from_list [ 1; 2 ]) then
    Ok ()
  else
    Error "expected larger set to be a superset"

let test_is_disjoint = fun _ctx ->
  if HashSet.is_disjoint (HashSet.from_list [ 1; 2 ]) (HashSet.from_list [ 3; 4 ]) then
    Ok ()
  else
    Error "expected disjoint sets to report disjoint"

let test_iter = fun _ctx ->
  if sort_ints (Iterator.to_list (HashSet.iter (HashSet.from_list [ 1; 2; 3 ]))) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected iter to yield each member exactly once"

let test_mut_iter = fun _ctx ->
  if
    sort_ints (MutIterator.to_list (HashSet.mut_iter (HashSet.from_list [ 1; 2; 3 ]))) = [ 1; 2; 3 ]
  then
    Ok ()
  else
    Error "expected mut_iter to yield each member exactly once"

let tests =
  Test.[
    case "HashSet.create starts empty" test_create;
    case "HashSet.with_capacity starts empty" test_with_capacity;
    case "HashSet.from_list deduplicates members" test_from_list_with_duplicates;
    case "HashSet.insert new value returns true" test_insert_new;
    case "HashSet.insert duplicate value returns false" test_insert_duplicate;
    case "HashSet.remove existing value returns true" test_remove_existing;
    case "HashSet.remove missing value returns false" test_remove_missing;
    case "HashSet.contains reflects membership" test_contains;
    case "HashSet.length counts unique members" test_length_after_duplicate_inserts;
    case "HashSet.clear removes all members" test_clear;
    case "HashSet.for_each visits each member once" test_for_each;
    case "HashSet.fold_left accumulates all members" test_fold_left;
    case "HashSet.to_list contains every member once" test_to_list;
    case "HashSet.union contains distinct members from both sides" test_union;
    case "HashSet.intersection keeps shared members" test_intersection;
    case "HashSet.difference keeps left-only members" test_difference;
    case "HashSet.symmetric_difference keeps non-overlapping members" test_symmetric_difference;
    case "HashSet.is_subset true case" test_is_subset_true;
    case "HashSet.is_subset false case" test_is_subset_false;
    case "HashSet.is_superset true case" test_is_superset_true;
    case "HashSet.is_disjoint detects non-overlapping sets" test_is_disjoint;
    case "HashSet.iter yields each member once" test_iter;
    case "HashSet.mut_iter yields each member once" test_mut_iter;
  ]

let main ~args = Test.Cli.main ~name:"hashset" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
