(** Unit tests for Shrinker module *)
open Std
open Propane

let test_towards_target_only_moves_closer = fun _ctx ->
  let shrinker = Shrinker.towards 50 in
  let original = 100 in
  let shrunk = Shrinker.shrink shrinker original in
  if List.all shrunk ~fn:(fun candidate -> Int.abs (candidate - 50) < Int.abs (original - 50)) then
    Ok ()
  else
    Error "towards produced a candidate that was not closer to the target"

let test_int_shrinking_is_finite = fun _ctx ->
  let rec loop value remaining =
    if remaining = 0 then
      Error "int shrinking did not terminate"
    else
      match Shrinker.shrink Shrinker.int value with
      | [] -> Ok ()
      | next :: _ -> loop next (remaining - 1)
  in
  loop 100_000 200

let test_string_shrinker_never_returns_longer_values = fun _ctx ->
  let original = "hello world" in
  let shrunk = Shrinker.shrink Shrinker.string original in
  if List.all shrunk ~fn:(fun candidate -> String.length candidate <= String.length original) then
    Ok ()
  else
    Error "string shrinker returned a longer value"

let test_string_shrinker_can_simplify_characters = fun _ctx ->
  let shrunk = Shrinker.shrink Shrinker.string "z" in
  if List.any shrunk ~fn:(fun candidate -> String.length candidate = 1 && candidate != "z") then
    Ok ()
  else
    Error "string shrinker did not produce any single-character simplification"

let test_list_shrinker_removes_elements = fun _ctx ->
  let original = [ 1; 2; 3; 4; ] in
  let shrunk = Shrinker.shrink (Shrinker.list Shrinker.nil) original in
  if List.any shrunk ~fn:(fun candidate -> List.length candidate < List.length original) then
    Ok ()
  else
    Error "list shrinker did not remove any elements"

let test_list_shrinker_uses_the_element_shrinker = fun _ctx ->
  let original = [ 10 ] in
  let shrunk = Shrinker.shrink (Shrinker.list (Shrinker.towards 0)) original in
  if List.contains shrunk ~value:[ 0 ] then
    Ok ()
  else
    Error "list shrinker did not use the provided element shrinker"

let test_hashmap_shrinker_shrinks_keys_and_values = fun _ctx ->
  let original = Collections.HashMap.from_list [ (10, 20); ] in
  let shrunk = Shrinker.shrink (Shrinker.hashmap (Shrinker.towards 0) (Shrinker.towards 0)) original in
  if
    List.any
      shrunk
      ~fn:(fun candidate ->
        Collections.HashMap.get candidate ~key:0 = Some 20
        || Collections.HashMap.get candidate ~key:10 = Some 0
        || Collections.HashMap.get candidate ~key:0 = Some 0)
  then
    Ok ()
  else
    Error "hashmap shrinker did not shrink keys or values"

let test_option_shrinker_can_drop_the_payload = fun _ctx ->
  let shrunk = Shrinker.shrink (Shrinker.option Shrinker.int) (Some 10) in
  if List.contains shrunk ~value:None then
    Ok ()
  else
    Error "option shrinker did not produce None"

let tests =
  Test.[
    case "towards target only moves closer" test_towards_target_only_moves_closer;
    case "int shrinking is finite" test_int_shrinking_is_finite;
    case
      "string shrinker never returns longer values"
      test_string_shrinker_never_returns_longer_values;
    case "string shrinker can simplify characters" test_string_shrinker_can_simplify_characters;
    case "list shrinker removes elements" test_list_shrinker_removes_elements;
    case "list shrinker uses the element shrinker" test_list_shrinker_uses_the_element_shrinker;
    case "hashmap shrinker shrinks keys and values" test_hashmap_shrinker_shrinks_keys_and_values;
    case "option shrinker can drop the payload" test_option_shrinker_can_drop_the_payload;
  ]

let main ~args = Test.Cli.main ~name:"propane/shrinker_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
