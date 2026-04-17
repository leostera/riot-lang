open Std
module Queue = Collections.Queue
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

type 'a box = {
  mutable value: 'a;
}

let box = fun value -> { value }

let test_create = fun _ctx ->
  let queue = Queue.create () in
  if Queue.is_empty queue then Ok () else Error "expected Queue.create to start empty"

let test_with_capacity = fun _ctx ->
  let queue = Queue.with_capacity ~size:4 in
  if Queue.is_empty queue then Ok () else Error "expected Queue.with_capacity to start empty"

let test_from_list = fun _ctx ->
  if Queue.to_list (Queue.from_list [ 1; 2; 3 ]) = [ 1; 2; 3 ] then Ok ()
  else Error "expected Queue.from_list to preserve FIFO order"

let test_push_then_front = fun _ctx ->
  let queue = Queue.create () in
  Queue.push queue ~value:1;
  Queue.push queue ~value:2;
  if Queue.front queue = Some 1 then Ok () else Error "expected front to return earliest pushed value"

let test_pop_empty = fun _ctx ->
  if Queue.pop (Queue.create ()) = None then Ok () else Error "expected Queue.pop empty = None"

let test_pop_fifo = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  match Queue.pop queue, Queue.pop queue, Queue.pop queue, Queue.pop queue with
  | Some 1, Some 2, Some 3, None -> Ok ()
  | _ -> Error "expected Queue.pop to return values in FIFO order"

let test_length_after_push_pop = fun _ctx ->
  let queue = Queue.create () in
  Queue.push queue ~value:1;
  Queue.push queue ~value:2;
  ignore (Queue.pop queue);
  if Int.equal (Queue.length queue) 1 then Ok () else Error "expected Queue.length to track live items"

let test_is_empty_after_removing_all = fun _ctx ->
  let queue = Queue.from_list [ 1 ] in
  ignore (Queue.pop queue);
  if Queue.is_empty queue then Ok () else Error "expected queue to be empty after removing all items"

let test_clear = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  Queue.clear queue;
  if Queue.is_empty queue then Ok () else Error "expected Queue.clear to empty queue"

let test_for_each = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  let seen = box [] in
  Queue.for_each queue ~fn:(fun value -> seen.value <- value :: seen.value);
  if List.reverse seen.value = [ 1; 2; 3 ] then Ok ()
  else Error "expected Queue.for_each to preserve FIFO order"

let test_fold_left = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  if String.equal (Queue.fold_left queue ~acc:"" ~fn:(fun acc value -> acc ^ Int.to_string value)) "123" then Ok ()
  else Error "expected Queue.fold_left to preserve FIFO order"

let test_to_list = fun _ctx ->
  if Queue.to_list (Queue.from_list [ 1; 2; 3 ]) = [ 1; 2; 3 ] then Ok ()
  else Error "expected Queue.to_list to preserve FIFO order"

let test_contains = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  if Queue.contains queue ~value:2 && not (Queue.contains queue ~value:9) then Ok ()
  else Error "expected contains to reflect membership"

let test_append = fun _ctx ->
  let left = Queue.from_list [ 1; 2 ] in
  let right = Queue.from_list [ 3; 4 ] in
  Queue.append left right;
  if Queue.to_list left = [ 1; 2; 3; 4 ] && Queue.is_empty right then Ok ()
  else Error "expected append to move right values to left and clear right"

let test_transfer = fun _ctx ->
  let src = Queue.from_list [ 1; 2 ] in
  let dst = Queue.from_list [ 3 ] in
  Queue.transfer ~src ~dst;
  if Queue.to_list dst = [ 3; 1; 2 ] && Queue.is_empty src then Ok ()
  else Error "expected transfer to move src values into dst and clear src"

let test_iter = fun _ctx ->
  if Iterator.to_list (Queue.iter (Queue.from_list [ 1; 2; 3 ])) = [ 1; 2; 3 ] then Ok ()
  else Error "expected Queue.iter to preserve FIFO order"

let test_mut_iter = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  let items = MutIterator.to_list (Queue.mut_iter queue) in
  if items = [ 1; 2; 3 ] && Queue.is_empty queue then Ok ()
  else Error "expected Queue.mut_iter to drain queue in FIFO order"

let tests =
  Test.[
    case "Queue.create starts empty" test_create;
    case "Queue.with_capacity starts empty" test_with_capacity;
    case "Queue.from_list preserves FIFO order" test_from_list;
    case "Queue.push then front returns earliest value" test_push_then_front;
    case "Queue.pop on empty returns None" test_pop_empty;
    case "Queue.pop preserves FIFO order" test_pop_fifo;
    case "Queue.length tracks push/pop sequence" test_length_after_push_pop;
    case "Queue.is_empty after removing all items" test_is_empty_after_removing_all;
    case "Queue.clear empties queue" test_clear;
    case "Queue.for_each preserves FIFO order" test_for_each;
    case "Queue.fold_left preserves FIFO order" test_fold_left;
    case "Queue.to_list preserves FIFO order" test_to_list;
    case "Queue.contains reflects membership" test_contains;
    case "Queue.append moves right into left and clears right" test_append;
    case "Queue.transfer moves src into dst and clears src" test_transfer;
    case "Queue.iter yields FIFO order" test_iter;
    case "Queue.mut_iter drains in FIFO order" test_mut_iter;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"queue" ~tests ~args) ~args:Env.args ()
