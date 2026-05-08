open Std

module Test = Std.Test
module TypedKeyHashMap = Std.Collections.TypedKeyHashMap

let test_typed_key_hashmap_roundtrips_distinct_types = fun _ctx ->
  let map = TypedKeyHashMap.create () in
  let name_key = TypedKeyHashMap.key () in
  let count_key = TypedKeyHashMap.key () in
  let _ = TypedKeyHashMap.insert map ~key:name_key ~value:"suri" in
  let _ = TypedKeyHashMap.insert map ~key:count_key ~value:42 in
  Test.assert_equal ~expected:(Some "suri") ~actual:(TypedKeyHashMap.get map ~key:name_key);
  Test.assert_equal ~expected:(Some 42) ~actual:(TypedKeyHashMap.get map ~key:count_key);
  Ok ()

let test_typed_key_hashmap_keeps_same_type_keys_distinct = fun _ctx ->
  let map = TypedKeyHashMap.create () in
  let first_key = TypedKeyHashMap.key () in
  let second_key = TypedKeyHashMap.key () in
  let _ = TypedKeyHashMap.insert map ~key:first_key ~value:"first" in
  Test.assert_equal ~expected:(Some "first") ~actual:(TypedKeyHashMap.get map ~key:first_key);
  Test.assert_equal ~expected:None ~actual:(TypedKeyHashMap.get map ~key:second_key);
  Ok ()

let test_typed_key_hashmap_insert_and_remove_return_typed_previous_values = fun _ctx ->
  let map = TypedKeyHashMap.create () in
  let key = TypedKeyHashMap.key () in
  Test.assert_equal ~expected:None ~actual:(TypedKeyHashMap.insert map ~key ~value:1);
  Test.assert_equal ~expected:(Some 1) ~actual:(TypedKeyHashMap.insert map ~key ~value:2);
  Test.assert_equal ~expected:(Some 2) ~actual:(TypedKeyHashMap.remove map ~key);
  Test.assert_equal ~expected:None ~actual:(TypedKeyHashMap.get map ~key);
  Ok ()

let test_typed_key_hashmap_entry_reflects_membership = fun _ctx ->
  let map = TypedKeyHashMap.create () in
  let key = TypedKeyHashMap.key () in
  (
    match TypedKeyHashMap.entry map ~key with
    | TypedKeyHashMap.Vacant -> ()
    | TypedKeyHashMap.Occupied _ -> panic "expected vacant typed-key hashmap entry"
  );
  let _ = TypedKeyHashMap.insert map ~key ~value:"value" in
  (
    match TypedKeyHashMap.entry map ~key with
    | TypedKeyHashMap.Occupied value -> Test.assert_equal ~expected:"value" ~actual:value
    | TypedKeyHashMap.Vacant -> panic "expected occupied typed-key hashmap entry"
  );
  Ok ()

let test_typed_key_hashmap_from_list_and_traversal = fun _ctx ->
  let name_key = TypedKeyHashMap.key () in
  let count_key = TypedKeyHashMap.key () in
  let map =
    TypedKeyHashMap.from_list
      [
        TypedKeyHashMap.Binding (name_key, "riot");
        TypedKeyHashMap.Binding (count_key, 2);
      ]
  in
  Test.assert_equal ~expected:2 ~actual:(TypedKeyHashMap.length map);
  Test.assert_true (TypedKeyHashMap.has_key map ~key:name_key);
  Test.assert_true (TypedKeyHashMap.has_key map ~key:count_key);
  let seen = ref 0 in
  TypedKeyHashMap.for_each map ~fn:(fun _key _binding -> seen := !seen + 1);
  Test.assert_equal ~expected:2 ~actual:!seen;
  Ok ()

let tests =
  Test.[
    case
      "TypedKeyHashMap roundtrips distinct types"
      test_typed_key_hashmap_roundtrips_distinct_types;
    case
      "TypedKeyHashMap keeps same-type keys distinct"
      test_typed_key_hashmap_keeps_same_type_keys_distinct;
    case
      "TypedKeyHashMap insert and remove return typed previous values"
      test_typed_key_hashmap_insert_and_remove_return_typed_previous_values;
    case
      "TypedKeyHashMap entry reflects membership"
      test_typed_key_hashmap_entry_reflects_membership;
    case
      "TypedKeyHashMap from_list and traversal mirror HashMap"
      test_typed_key_hashmap_from_list_and_traversal;
  ]

let main ~args = Test.Cli.main ~name:"std_typed_key_hashmap_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
