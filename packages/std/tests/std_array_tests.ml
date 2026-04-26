open Std

module Array = Collections.Array
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

type 'a box = { mutable value: 'a }

let box = fun value -> { value }

let test_make = fun _ctx ->
  if Array.to_list (Array.make ~count:3 ~value:7) = [ 7; 7; 7 ] then
    Ok ()
  else
    Error "expected Array.make to fill every slot"

let test_init = fun _ctx ->
  if Array.to_list (Array.init ~count:4 ~fn:(fun idx -> idx * idx)) = [ 0; 1; 4; 9; ] then
    Ok ()
  else
    Error "expected Array.init to map indices"

let test_length = fun _ctx ->
  if Int.equal (Array.length [|1; 2; 3|]) 3 then
    Ok ()
  else
    Error "expected array length = 3"

let test_get_valid = fun _ctx ->
  match Array.get [|3; 4; 5|] ~at:1 with
  | Some 4 -> Ok ()
  | _ -> Error "expected Array.get valid index to return value"

let test_get_negative = fun _ctx ->
  match Array.get [|3; 4; 5|] ~at:(-1) with
  | None -> Ok ()
  | Some _ -> Error "expected Array.get negative index = None"

let test_get_past_end = fun _ctx ->
  match Array.get [|3; 4; 5|] ~at:3 with
  | None -> Ok ()
  | Some _ -> Error "expected Array.get past end = None"

let test_get_unchecked = fun _ctx ->
  if Int.equal (Array.get_unchecked [|3; 4; 5|] ~at:2) 5 then
    Ok ()
  else
    Error "expected Array.get_unchecked valid index to return value"

let test_set = fun _ctx ->
  let values = [|1; 2; 3|] in
  Array.set values ~at:1 ~value:9;
  if Array.to_list values = [ 1; 9; 3 ] then
    Ok ()
  else
    Error "expected Array.set to mutate one slot"

let test_clone = fun _ctx ->
  let original = [|1; 2; 3|] in
  let clone = Array.clone original in
  Array.set original ~at:1 ~value:9;
  if Array.to_list clone = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Array.clone to detach copies"

let test_blit_non_overlapping = fun _ctx ->
  let src = [|1; 2; 3|] in
  let dst = [|0; 0; 0; 0; 0|] in
  Array.blit src ~src_offset:0 ~dst ~dst_offset:1 ~len:3;
  if Array.to_list dst = [ 0; 1; 2; 3; 0; ] then
    Ok ()
  else
    Error "expected non-overlapping blit to copy into destination"

let test_blit_overlapping_right = fun _ctx ->
  let values = [|1; 2; 3; 4|] in
  Array.blit values ~src_offset:0 ~dst:values ~dst_offset:1 ~len:3;
  if Array.to_list values = [ 1; 1; 2; 3; ] then
    Ok ()
  else
    Error "expected overlapping right-shift blit semantics"

let test_blit_overlapping_left = fun _ctx ->
  let values = [|1; 2; 3; 4|] in
  Array.blit values ~src_offset:1 ~dst:values ~dst_offset:0 ~len:3;
  if Array.to_list values = [ 2; 3; 4; 4; ] then
    Ok ()
  else
    Error "expected overlapping left-shift blit semantics"

let test_sub_interior = fun _ctx ->
  if Array.to_list (Array.sub [|0; 1; 2; 3|] ~offset:1 ~len:2) = [ 1; 2 ] then
    Ok ()
  else
    Error "expected interior Array.sub slice"

let test_sub_zero_length = fun _ctx ->
  if Array.to_list (Array.sub [|0; 1; 2; 3|] ~offset:2 ~len:0) = [] then
    Ok ()
  else
    Error "expected zero-length Array.sub to be empty"

let test_for_each_order = fun _ctx ->
  let visited = box [] in
  Array.for_each [|1; 2; 3|] ~fn:(fun value -> visited.value <- value :: visited.value);
  if List.reverse visited.value = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Array.for_each to preserve index order"

let test_map = fun _ctx ->
  if Array.to_list (Array.map [|1; 2; 3|] ~fn:(fun value -> value * 2)) = [ 2; 4; 6 ] then
    Ok ()
  else
    Error "expected Array.map to build a fresh mapped array"

let test_fold_left = fun _ctx ->
  if Int.equal (Array.fold_left [|1; 2; 3|] ~init:0 ~fn:(fun acc value -> acc + value)) 6 then
    Ok ()
  else
    Error "expected Array.fold_left to accumulate left-to-right"

let test_fold_right = fun _ctx ->
  if
    String.equal
      (Array.fold_right [|1; 2; 3|] ~init:"" ~fn:(fun value acc -> Int.to_string value ^ acc))
      "123"
  then
    Ok ()
  else
    Error "expected Array.fold_right to accumulate right-to-left"

let test_from_list = fun _ctx ->
  if Array.to_list (Array.from_list [ 1; 2; 3 ]) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Array.from_list to preserve order"

let test_iter = fun _ctx ->
  if Iterator.to_list (Array.iter [|1; 2; 3|]) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Array.iter to expose immutable iteration in order"

let test_mut_iter = fun _ctx ->
  if MutIterator.to_list (Array.mut_iter [|1; 2; 3|]) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Array.mut_iter to expose mutable iteration in order"

let tests =
  Test.[
    case "Array.make fills every slot" test_make;
    case "Array.init maps indices" test_init;
    case "Array.length reports array size" test_length;
    case "Array.get valid index" test_get_valid;
    case "Array.get negative index" test_get_negative;
    case "Array.get past end index" test_get_past_end;
    case "Array.get_unchecked valid index" test_get_unchecked;
    case "Array.set mutates one slot" test_set;
    case "Array.clone detaches copies" test_clone;
    case "Array.blit non-overlapping copy" test_blit_non_overlapping;
    case "Array.blit overlapping right shift" test_blit_overlapping_right;
    case "Array.blit overlapping left shift" test_blit_overlapping_left;
    case "Array.sub interior slice" test_sub_interior;
    case "Array.sub zero length" test_sub_zero_length;
    case "Array.for_each preserves order" test_for_each_order;
    case "Array.map doubles values" test_map;
    case "Array.fold_left accumulates left-to-right" test_fold_left;
    case "Array.fold_right accumulates right-to-left" test_fold_right;
    case "Array.from_list preserves order" test_from_list;
    case "Array.iter yields items in order" test_iter;
    case "Array.mut_iter yields items in order" test_mut_iter;
  ]

let main ~args = Test.Cli.main ~name:"array" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
