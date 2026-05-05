open Std

module IndexSet = Collections.IndexSet
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

let expect_list = fun ~label ~expected ~actual ->
  if actual = expected then
    Ok ()
  else
    Error (label
    ^ ": expected ["
    ^ String.concat ", " (List.map expected ~fn:Int.to_string)
    ^ "] but got ["
    ^ String.concat ", " (List.map actual ~fn:Int.to_string)
    ^ "]")

let test_from_list_preserves_first_insert_order = fun _ctx ->
  let set = IndexSet.from_list [ 3; 1; 3; 2; 1; ] in
  expect_list ~label:"from_list order" ~expected:[ 3; 1; 2; ] ~actual:(IndexSet.to_list set)

let test_insert_appends_new_values_only = fun _ctx ->
  let set = IndexSet.create () in
  let first = IndexSet.insert set ~value:2 in
  let duplicate = IndexSet.insert set ~value:2 in
  let second = IndexSet.insert set ~value:4 in
  if first && not duplicate && second then
    expect_list ~label:"insert order" ~expected:[ 2; 4; ] ~actual:(IndexSet.to_list set)
  else
    Error "expected insert to report new values only"

let test_remove_preserves_remaining_order = fun _ctx ->
  let set = IndexSet.from_list [ 1; 2; 3; 4; ] in
  if IndexSet.remove set ~value:2 then
    expect_list ~label:"remove order" ~expected:[ 1; 3; 4; ] ~actual:(IndexSet.to_list set)
  else
    Error "expected remove existing value to return true"

let test_union_preserves_left_then_new_right_order = fun _ctx ->
  let actual =
    IndexSet.union (IndexSet.from_list [ 3; 1; ]) (IndexSet.from_list [ 1; 4; 2; ])
    |> IndexSet.to_list
  in
  expect_list ~label:"union order" ~expected:[ 3; 1; 4; 2; ] ~actual

let test_intersection_preserves_left_order = fun _ctx ->
  let actual =
    IndexSet.intersection (IndexSet.from_list [ 4; 1; 3; 2; ]) (IndexSet.from_list [ 1; 2; 4; ])
    |> IndexSet.to_list
  in
  expect_list ~label:"intersection order" ~expected:[ 4; 1; 2; ] ~actual

let test_difference_preserves_left_order = fun _ctx ->
  let actual =
    IndexSet.difference (IndexSet.from_list [ 4; 1; 3; 2; ]) (IndexSet.from_list [ 1; 2; ])
    |> IndexSet.to_list
  in
  expect_list ~label:"difference order" ~expected:[ 4; 3; ] ~actual

let test_symmetric_difference_preserves_side_order = fun _ctx ->
  let actual =
    IndexSet.symmetric_difference
      (IndexSet.from_list [ 4; 1; 3; ])
      (IndexSet.from_list [ 1; 2; 5; ])
    |> IndexSet.to_list
  in
  expect_list ~label:"symmetric difference order" ~expected:[ 4; 3; 2; 5; ] ~actual

let test_iterators_preserve_order = fun _ctx ->
  let set = IndexSet.from_list [ 7; 8; 7; 9; ] in
  let iterator_values = Iterator.to_list (IndexSet.iter set) in
  let mut_iterator_values = MutIterator.to_list (IndexSet.mut_iter set) in
  match (
    expect_list ~label:"iter order" ~expected:[ 7; 8; 9; ] ~actual:iterator_values,
    expect_list ~label:"mut_iter order" ~expected:[ 7; 8; 9; ] ~actual:mut_iterator_values
  ) with
  | (Ok (), Ok ()) -> Ok ()
  | (Error err, _)
  | (_, Error err) -> Error err

let tests =
  Test.[
    case
      "IndexSet.from_list preserves first insertion order"
      test_from_list_preserves_first_insert_order;
    case "IndexSet.insert appends new values only" test_insert_appends_new_values_only;
    case "IndexSet.remove preserves remaining order" test_remove_preserves_remaining_order;
    case
      "IndexSet.union preserves left then new right order"
      test_union_preserves_left_then_new_right_order;
    case "IndexSet.intersection preserves left order" test_intersection_preserves_left_order;
    case "IndexSet.difference preserves left order" test_difference_preserves_left_order;
    case
      "IndexSet.symmetric_difference preserves side order"
      test_symmetric_difference_preserves_side_order;
    case "IndexSet.iterators preserve order" test_iterators_preserve_order;
  ]

let main ~args = Test.Cli.main ~name:"indexset" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
