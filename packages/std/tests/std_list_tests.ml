open Std

type 'a box = {
  mutable value: 'a;
}

let box = fun value -> { value }

let test_length = fun _ctx ->
  if Int.equal (List.length []) 0 then
    Ok ()
  else
    Error "expected List.length [] = 0"

let test_is_empty = fun _ctx ->
  if List.is_empty [] && not (List.is_empty [ 1 ]) then
    Ok ()
  else
    Error "expected List.is_empty to reflect list contents"

let test_append = fun _ctx ->
  if List.append [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ] then
    Ok ()
  else
    Error "expected List.append to concatenate inputs"

let test_reverse = fun _ctx ->
  if List.reverse [ 1; 2; 3 ] = [ 3; 2; 1 ] then
    Ok ()
  else
    Error "expected List.reverse to invert order"

let test_concat = fun _ctx ->
  let actual = List.concat [ [ "a"; "b" ]; [ "c" ]; [ "d"; "e" ] ] in
  if actual = [ "a"; "b"; "c"; "d"; "e" ] then
    Ok ()
  else
    Error "expected List.concat to preserve nested list order"

let test_init = fun _ctx ->
  if List.init ~count:5 ~fn:(fun idx -> idx) = [ 0; 1; 2; 3; 4 ] then
    Ok ()
  else
    Error "expected List.init to map indices"

let test_head_none = fun _ctx ->
  if List.head [] = None then
    Ok ()
  else
    Error "expected List.head [] = None"

let test_head_some = fun _ctx ->
  if List.head [ 42 ] = Some 42 then
    Ok ()
  else
    Error "expected List.head [42] = Some 42"

let test_tail_empty = fun _ctx ->
  if List.tail [] = [] then
    Ok ()
  else
    Error "expected List.tail [] = []"

let test_get_valid = fun _ctx ->
  if List.get [ 1; 2; 3 ] ~at:1 = Some 2 then
    Ok ()
  else
    Error "expected List.get valid index = Some value"

let test_get_negative = fun _ctx ->
  if List.get [ 1; 2; 3 ] ~at:(-1) = None then
    Ok ()
  else
    Error "expected List.get negative index = None"

let test_get_unchecked = fun _ctx ->
  if Int.equal (List.get_unchecked [ 1; 2; 3 ] ~at:2) 3 then
    Ok ()
  else
    Error "expected List.get_unchecked valid index to return value"

let test_map = fun _ctx ->
  if List.map [ 1; 2; 3 ] ~fn:(fun value -> value * value) = [ 1; 4; 9 ] then
    Ok ()
  else
    Error "expected List.map to transform every value"

let test_for_each_order = fun _ctx ->
  let visited = box [] in
  List.for_each [ 1; 2; 3 ] ~fn:(fun value -> visited.value <- value :: visited.value);
  if List.reverse visited.value = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected List.for_each to preserve left-to-right order"

let test_fold_left = fun _ctx ->
  if Int.equal (List.fold_left [ 1; 2; 3 ] ~init:0 ~fn:(fun acc value -> acc - value)) (-6) then
    Ok ()
  else
    Error "expected List.fold_left to associate from the left"

let test_fold_right = fun _ctx ->
  if List.fold_right [ 1; 2; 3 ] ~init:[] ~fn:(fun value acc -> value :: acc) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected List.fold_right to rebuild original order"

let test_enumerate = fun _ctx ->
  if List.enumerate [ "a"; "b" ] = [ (0, "a"); (1, "b") ] then
    Ok ()
  else
    Error "expected List.enumerate to pair indices with values"

let test_all = fun _ctx ->
  if List.all [ 2; 4; 6 ] ~fn:(fun value -> value mod 2 = 0) then
    Ok ()
  else
    Error "expected List.all to succeed when every value matches"

let test_any = fun _ctx ->
  if List.any [ 1; 3; 4 ] ~fn:(fun value -> value mod 2 = 0) then
    Ok ()
  else
    Error "expected List.any to succeed when one value matches"

let test_contains = fun _ctx ->
  if List.contains [ 1; 2; 3 ] ~value:2 && not (List.contains [ 1; 2; 3 ] ~value:9) then
    Ok ()
  else
    Error "expected List.contains to reflect membership"

let test_find = fun _ctx ->
  if List.find [ 1; 3; 4; 6 ] ~fn:(fun value -> value mod 2 = 0) = Some 4 then
    Ok ()
  else
    Error "expected List.find to return first matching value"

let test_filter_map = fun _ctx ->
  if List.filter_map [ 1; 2; 3; 4 ]
      ~fn:(fun value ->
        if value mod 2 = 0 then
          Some (value * 10)
        else
          None) = [ 20; 40 ] then
    Ok ()
  else
    Error "expected List.filter_map to drop Nones and unwrap Somes"

let test_zip = fun _ctx ->
  if List.zip [ 1; 2 ] [ "a"; "b" ] = [ (1, "a"); (2, "b") ] then
    Ok ()
  else
    Error "expected List.zip to pair matching positions"

let test_unzip = fun _ctx ->
  if List.unzip [ (1, "a"); (2, "b") ] = ([ 1; 2 ], [ "a"; "b" ]) then
    Ok ()
  else
    Error "expected List.unzip to split pair lists"

let tests =
  Test.[
    case "List.length [] = 0" test_length;
    case "List.is_empty reflects list contents" test_is_empty;
    case "List.append concatenates inputs" test_append;
    case "List.reverse flips order" test_reverse;
    case "List.concat preserves input order" test_concat;
    case "List.init maps indices" test_init;
    case "List.head [] returns None" test_head_none;
    case "List.head singleton returns Some value" test_head_some;
    case "List.tail [] returns []" test_tail_empty;
    case "List.get valid index returns Some value" test_get_valid;
    case "List.get negative index returns None" test_get_negative;
    case "List.get_unchecked valid index returns value" test_get_unchecked;
    case "List.map transforms every value" test_map;
    case "List.for_each preserves left-to-right order" test_for_each_order;
    case "List.fold_left subtracts left-associatively" test_fold_left;
    case "List.fold_right can rebuild the original order" test_fold_right;
    case "List.enumerate pairs indices and values" test_enumerate;
    case "List.all returns true when every value matches" test_all;
    case "List.any returns true when one value matches" test_any;
    case "List.contains reflects membership" test_contains;
    case "List.find returns first matching value" test_find;
    case "List.filter_map drops Nones and unwraps Somes" test_filter_map;
    case "List.zip pairs matching positions" test_zip;
    case "List.unzip splits pair lists" test_unzip;
  ]

let main ~args = Test.Cli.main ~name:"list" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
