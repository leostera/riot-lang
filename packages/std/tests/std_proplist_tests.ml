open Std

module Proplist = Collections.Proplist
module Iterator = Iter.Iterator

let test_empty = fun _ctx ->
  if Proplist.is_empty Proplist.empty && Int.equal (Proplist.length Proplist.empty) 0 then
    Ok ()
  else
    Error "expected Proplist.empty to be empty"

let test_get_returns_first_matching_binding = fun _ctx ->
  match Proplist.get [ ("color", "red"); ("size", "l"); ("color", "blue"); ] ~key:"color" with
  | Some "red" -> Ok ()
  | _ -> Error "expected Proplist.get to return the first matching binding"

let test_get_all_preserves_binding_order = fun _ctx ->
  if
    Proplist.get_all [ ("color", "red"); ("size", "l"); ("color", "blue"); ] ~key:"color"
    = [ "red"; "blue" ]
  then
    Ok ()
  else
    Error "expected Proplist.get_all to return all matching values in order"

let test_add_prepends_new_binding = fun _ctx ->
  if
    Proplist.add [ ("size", "l"); ] ~key:"color" ~value:"red" = [ ("color", "red"); ("size", "l"); ]
  then
    Ok ()
  else
    Error "expected Proplist.add to prepend the new binding"

let test_set_replaces_existing_bindings_in_place = fun _ctx ->
  if
    Proplist.set [ ("color", "red"); ("size", "l"); ("color", "blue"); ] ~key:"color" ~value:"green"
    = [ ("color", "green"); ("size", "l"); ]
  then
    Ok ()
  else
    Error "expected Proplist.set to replace all existing bindings while preserving order"

let test_set_appends_missing_binding = fun _ctx ->
  if
    Proplist.set [ ("size", "l"); ] ~key:"color" ~value:"red" = [ ("size", "l"); ("color", "red"); ]
  then
    Ok ()
  else
    Error "expected Proplist.set to append a missing binding"

let test_remove_drops_all_matching_bindings = fun _ctx ->
  if
    Proplist.remove [ ("color", "red"); ("size", "l"); ("color", "blue"); ] ~key:"color"
    = [ ("size", "l"); ]
  then
    Ok ()
  else
    Error "expected Proplist.remove to drop every matching binding"

let test_has_key = fun _ctx ->
  let proplist = [ ("color", "red"); ("size", "l"); ] in
  if Proplist.has_key proplist ~key:"color" && not (Proplist.has_key proplist ~key:"shape") then
    Ok ()
  else
    Error "expected Proplist.has_key to reflect key membership"

let test_keys_and_values_preserve_pair_order = fun _ctx ->
  let proplist = [ ("color", "red"); ("size", "l"); ("color", "blue"); ] in
  if
    Proplist.keys proplist = [ "color"; "size"; "color" ]
    && Proplist.values proplist = [ "red"; "l"; "blue" ]
  then
    Ok ()
  else
    Error "expected Proplist.keys and Proplist.values to preserve pair order"

let test_fold_left_visits_pairs_in_order = fun _ctx ->
  let actual =
    Proplist.fold_left
      [ ("color", "red"); ("size", "l"); ]
      ~init:""
      ~fn:(fun acc key value -> acc ^ key ^ "=" ^ value ^ ";")
  in
  if String.equal actual "color=red;size=l;" then
    Ok ()
  else
    Error "expected Proplist.fold_left to visit pairs in order"

let test_iter_yields_pairs_in_order = fun _ctx ->
  let actual =
    Proplist.iter [ ("color", "red"); ("size", "l"); ("color", "blue"); ]
    |> Iterator.to_list
  in
  if actual = [ ("color", "red"); ("size", "l"); ("color", "blue"); ] then
    Ok ()
  else
    Error "expected Proplist.iter to yield pairs in order"

let tests =
  Test.[
    case "Proplist.empty is empty" test_empty;
    case "Proplist.get returns the first matching binding" test_get_returns_first_matching_binding;
    case "Proplist.get_all preserves binding order" test_get_all_preserves_binding_order;
    case "Proplist.add prepends a new binding" test_add_prepends_new_binding;
    case
      "Proplist.set replaces existing bindings in place"
      test_set_replaces_existing_bindings_in_place;
    case "Proplist.set appends a missing binding" test_set_appends_missing_binding;
    case "Proplist.remove drops all matching bindings" test_remove_drops_all_matching_bindings;
    case "Proplist.has_key reflects key membership" test_has_key;
    case
      "Proplist.keys and Proplist.values preserve pair order"
      test_keys_and_values_preserve_pair_order;
    case "Proplist.fold_left visits pairs in order" test_fold_left_visits_pairs_in_order;
    case "Proplist.iter yields pairs in order" test_iter_yields_pairs_in_order;
  ]

let main ~args = Test.Cli.main ~name:"proplist" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
