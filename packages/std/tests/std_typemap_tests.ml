open Std

module Test = Std.Test
module TypeMap = Std.Collections.TypeMap

let test_typemap_roundtrips_distinct_types = fun _ctx ->
  let map = TypeMap.create () in
  let name_key = TypeMap.key () in
  let count_key = TypeMap.key () in
  let _ = TypeMap.insert map ~key:name_key ~value:"suri" in
  let _ = TypeMap.insert map ~key:count_key ~value:42 in
  Test.assert_equal ~expected:(Some "suri") ~actual:(TypeMap.get map ~key:name_key);
  Test.assert_equal ~expected:(Some 42) ~actual:(TypeMap.get map ~key:count_key);
  Ok ()

let test_typemap_keeps_same_type_keys_distinct = fun _ctx ->
  let map = TypeMap.create () in
  let first_key = TypeMap.key () in
  let second_key = TypeMap.key () in
  let _ = TypeMap.insert map ~key:first_key ~value:"first" in
  Test.assert_equal ~expected:(Some "first") ~actual:(TypeMap.get map ~key:first_key);
  Test.assert_equal ~expected:None ~actual:(TypeMap.get map ~key:second_key);
  Ok ()

let test_typemap_insert_and_remove_return_typed_previous_values = fun _ctx ->
  let map = TypeMap.create () in
  let key = TypeMap.key () in
  Test.assert_equal ~expected:None ~actual:(TypeMap.insert map ~key ~value:1);
  Test.assert_equal ~expected:(Some 1) ~actual:(TypeMap.insert map ~key ~value:2);
  Test.assert_equal ~expected:(Some 2) ~actual:(TypeMap.remove map ~key);
  Test.assert_equal ~expected:None ~actual:(TypeMap.get map ~key);
  Ok ()

let test_typemap_entry_or_insert_and_modify = fun _ctx ->
  let map = TypeMap.create () in
  let key = TypeMap.key () in
  (
    match TypeMap.entry map ~key with
    | TypeMap.Vacant -> ()
    | TypeMap.Occupied _ -> panic "expected vacant typemap entry"
  );
  Test.assert_equal ~expected:"default" ~actual:(TypeMap.or_insert map ~key ~default:"default");
  TypeMap.and_modify map ~key ~fn:(fun value -> value ^ "-changed");
  (
    match TypeMap.entry map ~key with
    | TypeMap.Occupied value -> Test.assert_equal ~expected:"default-changed" ~actual:value
    | TypeMap.Vacant -> panic "expected occupied typemap entry"
  );
  Ok ()

let test_typemap_from_list_and_traversal = fun _ctx ->
  let name_key = TypeMap.key () in
  let count_key = TypeMap.key () in
  let map =
    TypeMap.from_list
      [
        TypeMap.Binding (name_key, "riot");
        TypeMap.Binding (count_key, 2);
      ]
  in
  Test.assert_equal ~expected:2 ~actual:(TypeMap.length map);
  Test.assert_true (TypeMap.has_key map ~key:name_key);
  Test.assert_true (TypeMap.contains_key map count_key);
  let seen = ref 0 in
  TypeMap.for_each map ~fn:(fun _key _binding -> seen := !seen + 1);
  Test.assert_equal ~expected:2 ~actual:!seen;
  Ok ()

let tests =
  Test.[
    case "TypeMap roundtrips distinct types" test_typemap_roundtrips_distinct_types;
    case "TypeMap keeps same-type keys distinct" test_typemap_keeps_same_type_keys_distinct;
    case
      "TypeMap insert and remove return typed previous values"
      test_typemap_insert_and_remove_return_typed_previous_values;
    case "TypeMap entry helpers match HashMap API shape" test_typemap_entry_or_insert_and_modify;
    case "TypeMap from_list and traversal mirror HashMap" test_typemap_from_list_and_traversal;
  ]

let main ~args = Test.Cli.main ~name:"std_typemap_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
