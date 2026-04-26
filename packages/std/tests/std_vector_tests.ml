open Std

module Vector = Collections.Vector
module Array = Collections.Array
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

let test_create = fun _ctx ->
  let vector = Vector.create () in
  if Vector.is_empty vector && Int.equal (Vector.length vector) 0 then
    Ok ()
  else
    Error "expected Vector.create to start empty"

let test_with_capacity = fun _ctx ->
  let vector = Vector.with_capacity ~size:8 in
  if Vector.is_empty vector && Vector.capacity vector >= 8 then
    Ok ()
  else
    Error "expected Vector.with_capacity to reserve space"

let test_push_on_empty = fun _ctx ->
  let vector = Vector.create () in
  Vector.push vector ~value:1;
  if Vector.length vector = 1 && Vector.first vector = Some 1 && Vector.last vector = Some 1 then
    Ok ()
  else
    Error "expected first push to set length first and last"

let test_push_growth = fun _ctx ->
  let vector = Vector.with_capacity ~size:1 in
  let initial_capacity = Vector.capacity vector in
  List.for_each [ 1; 2; 3; 4; ] ~fn:(fun value -> Vector.push vector ~value);
  if
    Vector.capacity vector > initial_capacity
    && Array.to_list (Vector.to_array vector) = [ 1; 2; 3; 4; ]
  then
    Ok ()
  else
    Error "expected growth to preserve contents"

let test_pop_empty = fun _ctx ->
  if Vector.pop (Vector.create ()) = None then
    Ok ()
  else
    Error "expected Vector.pop empty = None"

let test_pop_non_empty = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3 ] in
  let popped = Vector.pop vector in
  let items = Array.to_list (Vector.to_array vector) in
  if popped = Some 3 && items = [ 1; 2 ] then
    Ok ()
  else
    Error "expected Vector.pop to remove last pushed value"

let test_insert_at_zero = fun _ctx ->
  let vector = Vector.from_list [ 2; 3 ] in
  Vector.insert vector ~at:0 ~value:1;
  if Array.to_list (Vector.to_array vector) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected insert at 0 to prepend"

let test_insert_at_length = fun _ctx ->
  let vector = Vector.from_list [ 1; 2 ] in
  Vector.insert vector ~at:2 ~value:3;
  if Array.to_list (Vector.to_array vector) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected insert at length to append"

let test_insert_middle = fun _ctx ->
  let vector = Vector.from_list [ 1; 3; 4 ] in
  Vector.insert vector ~at:1 ~value:2;
  if Array.to_list (Vector.to_array vector) = [ 1; 2; 3; 4; ] then
    Ok ()
  else
    Error "expected insert in middle to shift tail right"

let test_remove_out_of_bounds = fun _ctx ->
  if Vector.remove (Vector.from_list [ 1; 2 ]) ~at:3 = None then
    Ok ()
  else
    Error "expected remove out of bounds = None"

let test_remove_middle = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3 ] in
  let removed = Vector.remove vector ~at:1 in
  let items = Array.to_list (Vector.to_array vector) in
  if removed = Some 2 && items = [ 1; 3 ] then
    Ok ()
  else
    Error "expected remove middle to compact tail"

let test_get = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3 ] in
  if Vector.get vector ~at:1 = Some 2 && Vector.get vector ~at:3 = None then
    Ok ()
  else
    Error "expected get to handle valid and invalid indices"

let test_set = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3 ] in
  match Vector.set vector ~at:1 ~value:9 with
  | Ok () when Array.to_list (Vector.to_array vector) = [ 1; 9; 3 ] -> Ok ()
  | _ -> Error "expected set valid index to update vector"

let test_set_oob = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3 ] in
  match Vector.set vector ~at:3 ~value:9 with
  | Error (Vector.OutOfBoundsSet { length; at }) when length = 3 && at = 3 -> Ok ()
  | _ -> Error "expected set oob to report OutOfBoundsSet"

let test_clear = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3 ] in
  Vector.clear vector;
  if Vector.is_empty vector then
    Ok ()
  else
    Error "expected clear to empty vector"

let test_reserve = fun _ctx ->
  let vector = Vector.create () in
  Vector.reserve vector ~size:10;
  if Vector.capacity vector >= 10 then
    Ok ()
  else
    Error "expected reserve to increase capacity"

let test_append = fun _ctx ->
  let left = Vector.from_list [ 1; 2 ] in
  let right = Vector.from_list [ 3; 4 ] in
  Vector.append left right;
  if Array.to_list (Vector.to_array left) = [ 1; 2; 3; 4; ] && Vector.is_empty right then
    Ok ()
  else
    Error "expected append to move right into left and clear right"

let test_concat = fun _ctx ->
  let left = Vector.from_list [ 1; 2 ] in
  let right = Vector.from_list [ 3; 4 ] in
  let combined = Vector.concat left right in
  if
    Array.to_list (Vector.to_array combined) = [ 1; 2; 3; 4; ]
    && Array.to_list (Vector.to_array left) = [ 1; 2 ]
    && Array.to_list (Vector.to_array right) = [ 3; 4 ]
  then
    Ok ()
  else
    Error "expected concat to copy both vectors without mutating them"

let test_extend = fun _ctx ->
  let left = Vector.from_list [ 1; 2 ] in
  let right = Vector.from_list [ 3; 4 ] in
  Vector.extend left right;
  if
    Array.to_list (Vector.to_array left) = [ 1; 2; 3; 4; ]
    && Array.to_list (Vector.to_array right) = [ 3; 4 ]
  then
    Ok ()
  else
    Error "expected extend to copy right into left without clearing right"

let test_split_off = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3; 4; ] in
  let tail = Vector.split_off vector ~at:2 in
  if
    Array.to_list (Vector.to_array vector) = [ 1; 2 ]
    && Array.to_list (Vector.to_array tail) = [ 3; 4 ]
  then
    Ok ()
  else
    Error "expected split_off to divide prefix and tail"

let test_sort = fun _ctx ->
  let vector = Vector.from_list [ 3; 1; 2 ] in
  Vector.sort vector;
  if Array.to_list (Vector.to_array vector) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected sort to order values ascending"

let test_reverse = fun _ctx ->
  let vector = Vector.from_list [ 1; 2; 3 ] in
  Vector.reverse vector;
  if Array.to_list (Vector.to_array vector) = [ 3; 2; 1 ] then
    Ok ()
  else
    Error "expected reverse to flip vector order"

let test_iter = fun _ctx ->
  if Iterator.to_list (Vector.iter (Vector.from_list [ 1; 2; 3 ])) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected iter to yield vector items in order"

let test_mut_iter = fun _ctx ->
  if MutIterator.to_list (Vector.mut_iter (Vector.from_list [ 1; 2; 3 ])) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected mut_iter to yield vector items in order"

let tests =
  Test.[
    case "Vector.create starts empty" test_create;
    case "Vector.with_capacity reserves space" test_with_capacity;
    case "Vector.push on empty sets first and last" test_push_on_empty;
    case "Vector growth preserves contents" test_push_growth;
    case "Vector.pop on empty returns None" test_pop_empty;
    case "Vector.pop removes the last element" test_pop_non_empty;
    case "Vector.insert at 0 prepends" test_insert_at_zero;
    case "Vector.insert at length appends" test_insert_at_length;
    case "Vector.insert in the middle shifts the tail" test_insert_middle;
    case "Vector.remove out of bounds returns None" test_remove_out_of_bounds;
    case "Vector.remove middle compacts the tail" test_remove_middle;
    case "Vector.get handles valid and invalid indices" test_get;
    case "Vector.set updates a valid index" test_set;
    case "Vector.set reports out-of-bounds errors" test_set_oob;
    case "Vector.clear empties the vector" test_clear;
    case "Vector.reserve increases capacity" test_reserve;
    case "Vector.append moves right into left and clears right" test_append;
    case "Vector.concat copies both inputs" test_concat;
    case "Vector.extend copies right into left" test_extend;
    case "Vector.split_off divides prefix and tail" test_split_off;
    case "Vector.sort orders values ascending" test_sort;
    case "Vector.reverse flips value order" test_reverse;
    case "Vector.iter yields items in order" test_iter;
    case "Vector.mut_iter yields items in order" test_mut_iter;
  ]

let main ~args = Test.Cli.main ~name:"vector" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
