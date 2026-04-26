open Std

module Queue = Collections.Queue
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator
module HashSet = Collections.HashSet

type Message.t +=
  | Queue_test_go
  | Queue_producer_done of int
  | Queue_consumer_values of int list

type 'a box = { mutable value: 'a }

type operation =
  | Push of int
  | Pop
  | Clear

let box = fun value -> { value }

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 2) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let wait_for_go = fun () ->
  let _ =
    receive
      ~selector:(
        function
        | Queue_test_go -> `select ()
        | _ -> `skip
      )
      ~timeout:(Time.Duration.from_secs 2)
      ()
  in
  ()

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let rec ints_from start count =
  if count <= 0 then
    []
  else
    start :: ints_from (start + 1) (count - 1)

let rec flatten lists =
  match lists with
  | [] -> []
  | values :: rest -> values @ flatten rest

let drain = fun queue ->
  let rec loop acc =
    match Queue.pop queue with
    | None -> List.reverse acc
    | Some value -> loop (value :: acc)
  in
  loop []

let render_int_list = fun values ->
  "[" ^ String.concat ", " (List.map values ~fn:Int.to_string) ^ "]"

let render_int_option = function
  | None -> "None"
  | Some value -> "Some(" ^ Int.to_string value ^ ")"

let make_batches = fun counts ->
  let rec loop producer = function
    | [] -> []
    | count :: rest -> ints_from (producer * 1_000_000) count :: loop (producer + 1) rest
  in
  loop 0 counts

let spawn_producers = fun ~parent ~queue ~batches ~yield_every ~on_done ->
  let rec loop producer remaining_batches acc =
    match remaining_batches with
    | [] -> List.reverse acc
    | batch :: rest ->
        let pid =
          spawn
            (fun () ->
              wait_for_go ();
              batch
              |> List.enumerate
              |> List.for_each
                ~fn:(fun (index, value) ->
                  Queue.push queue ~value;
                  if index mod yield_every = 0 then
                    yield ());
              on_done ();
              send parent (Queue_producer_done producer);
              Ok ())
        in
        loop (producer + 1) rest (pid :: acc)
  in
  loop 0 batches []

let wait_for_producers = fun ~producer_count ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      match await
        ~what:"queue producer completion"
        (
          function
          | Queue_producer_done _ -> `select ()
          | _ -> `skip
        ) with
      | Ok () -> loop (remaining - 1)
      | Error _ as error -> error
  in
  loop producer_count

let collect_consumer_values = fun ~consumer_count ->
  let rec loop remaining acc =
    if remaining = 0 then
      Ok (List.reverse acc)
    else
      match await
        ~what:"queue consumer completion"
        (
          function
          | Queue_consumer_values values -> `select values
          | _ -> `skip
        ) with
      | Ok values -> loop (remaining - 1) (values :: acc)
      | Error _ as error -> error
  in
  loop consumer_count []

let has_expected_set = fun ~expected ~actual ->
  let expected_set = HashSet.from_list expected in
  let actual_set = HashSet.from_list actual in
  Int.equal (List.length actual) (List.length expected)
  && Int.equal (HashSet.length actual_set) (HashSet.length expected_set)
  && sort_ints actual = sort_ints expected

let preserves_per_producer_fifo = fun ~batches ~actual ->
  let rec loop producer remaining_batches =
    match remaining_batches with
    | [] -> true
    | batch :: rest ->
        let observed = List.filter actual ~fn:(fun value -> value / 1_000_000 = producer) in
        batch = observed && loop (producer + 1) rest
  in
  loop 0 batches

let test_create = fun _ctx ->
  let queue = Queue.create () in
  if Queue.is_empty queue then
    Ok ()
  else
    Error "expected Queue.create to start empty"

let test_with_capacity = fun _ctx ->
  let queue = Queue.with_capacity ~size:4 in
  if Queue.is_empty queue then
    Ok ()
  else
    Error "expected Queue.with_capacity to start empty"

let test_from_list = fun _ctx ->
  if Queue.to_list (Queue.from_list [ 1; 2; 3 ]) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Queue.from_list to preserve FIFO order"

let test_push_then_front = fun _ctx ->
  let queue = Queue.create () in
  Queue.push queue ~value:1;
  Queue.push queue ~value:2;
  if Queue.front queue = Some 1 then
    Ok ()
  else
    Error "expected front to return earliest pushed value"

let test_pop_empty = fun _ctx ->
  if Queue.pop (Queue.create ()) = None then
    Ok ()
  else
    Error "expected Queue.pop empty = None"

let test_pop_fifo = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  match (Queue.pop queue, Queue.pop queue, Queue.pop queue, Queue.pop queue) with
  | (Some 1, Some 2, Some 3, None) -> Ok ()
  | _ -> Error "expected Queue.pop to return values in FIFO order"

let test_length_after_push_pop = fun _ctx ->
  let queue = Queue.create () in
  Queue.push queue ~value:1;
  Queue.push queue ~value:2;
  ignore (Queue.pop queue);
  if Int.equal (Queue.length queue) 1 then
    Ok ()
  else
    Error "expected Queue.length to track live items"

let test_is_empty_after_removing_all = fun _ctx ->
  let queue = Queue.from_list [ 1 ] in
  ignore (Queue.pop queue);
  if Queue.is_empty queue then
    Ok ()
  else
    Error "expected queue to be empty after removing all items"

let test_clear = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  Queue.clear queue;
  if Queue.is_empty queue then
    Ok ()
  else
    Error "expected Queue.clear to empty queue"

let test_for_each = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  let seen = box [] in
  Queue.for_each queue ~fn:(fun value -> seen.value <- value :: seen.value);
  if List.reverse seen.value = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Queue.for_each to preserve FIFO order"

let test_fold_left = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  if
    String.equal
      (Queue.fold_left queue ~init:"" ~fn:(fun acc value -> acc ^ Int.to_string value))
      "123"
  then
    Ok ()
  else
    Error "expected Queue.fold_left to preserve FIFO order"

let test_to_list = fun _ctx ->
  if Queue.to_list (Queue.from_list [ 1; 2; 3 ]) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Queue.to_list to preserve FIFO order"

let test_contains = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  if Queue.contains queue ~value:2 && not (Queue.contains queue ~value:9) then
    Ok ()
  else
    Error "expected contains to reflect membership"

let test_append = fun _ctx ->
  let left = Queue.from_list [ 1; 2 ] in
  let right = Queue.from_list [ 3; 4 ] in
  Queue.append left right;
  if Queue.to_list left = [ 1; 2; 3; 4; ] && Queue.is_empty right then
    Ok ()
  else
    Error "expected append to move right values to left and clear right"

let test_transfer = fun _ctx ->
  let src = Queue.from_list [ 1; 2 ] in
  let dst = Queue.from_list [ 3 ] in
  Queue.transfer ~src ~dst;
  if Queue.to_list dst = [ 3; 1; 2 ] && Queue.is_empty src then
    Ok ()
  else
    Error "expected transfer to move src values into dst and clear src"

let test_iter = fun _ctx ->
  if Iterator.to_list (Queue.iter (Queue.from_list [ 1; 2; 3 ])) = [ 1; 2; 3 ] then
    Ok ()
  else
    Error "expected Queue.iter to preserve FIFO order"

let test_mut_iter = fun _ctx ->
  let queue = Queue.from_list [ 1; 2; 3 ] in
  let items = MutIterator.to_list (Queue.mut_iter queue) in
  if items = [ 1; 2; 3 ] && Queue.is_empty queue then
    Ok ()
  else
    Error "expected Queue.mut_iter to drain queue in FIFO order"

let test_regression_clear_pop_sequence = fun _ctx ->
  let operations = [
    Clear;
    Clear;
    Pop;
    Push (-3);
    Push (-21);
    Push 11;
    Pop;
    Push (-24);
    Push 0;
    Pop;
    Push 27;
    Push 6;
    Pop;
    Push 7;
    Push (-12);
    Pop;
    Clear;
    Pop;
    Push (-2);
    Clear;
    Pop;
    Push 21;
    Clear;
    Push 20;
    Pop;
    Pop;
    Push 16;
    Push 15;
    Push 19;
    Push 18;
    Push 3;
    Push 32;
    Push (-23);
    Push 3;
    Push (-19);
    Pop;
    Pop;
    Push (-13);
    Push 16;
    Push (-30);
    Push 22;
    Push 3;
    Push (-32);
    Pop;
    Push 5;
    Pop;
    Clear;
    Push (-10);
    Pop;
    Pop;
    Pop;
    Pop;
    Pop;
    Push 17;
    Clear;
    Pop;
    Push 25;
  ]
  in
  let queue = Queue.create () in
  let rec pop_model = function
    | [] -> (None, [])
    | value :: rest -> (Some value, rest)
  in
  let render_operation = function
    | Push value -> "Push(" ^ Int.to_string value ^ ")"
    | Pop -> "Pop"
    | Clear -> "Clear"
  in
  let rec loop step model remaining =
    match remaining with
    | [] ->
        if not (Queue.to_list queue = model) then
          Error ("final state: expected queue "
          ^ render_int_list model
          ^ ", got "
          ^ render_int_list (Queue.to_list queue))
        else if not (Int.equal (Queue.length queue) (List.length model)) then
          Error ("final state: expected length "
          ^ Int.to_string (List.length model)
          ^ ", got "
          ^ Int.to_string (Queue.length queue))
        else if not (Queue.is_empty queue = List.is_empty model) then
          Error ("final state: expected is_empty="
          ^ Bool.to_string (List.is_empty model)
          ^ ", got "
          ^ Bool.to_string (Queue.is_empty queue))
        else
          Ok ()
    | (Push value) :: rest ->
        Queue.push queue ~value;
        let next_model = model @ [ value ] in
        if not (Queue.to_list queue = next_model) then
          Error ("step "
          ^ Int.to_string step
          ^ " "
          ^ render_operation (Push value)
          ^ ": expected queue "
          ^ render_int_list next_model
          ^ ", got "
          ^ render_int_list (Queue.to_list queue))
        else if not (Int.equal (Queue.length queue) (List.length next_model)) then
          Error ("step "
          ^ Int.to_string step
          ^ " "
          ^ render_operation (Push value)
          ^ ": expected length "
          ^ Int.to_string (List.length next_model)
          ^ ", got "
          ^ Int.to_string (Queue.length queue))
        else
          loop (step + 1) next_model rest
    | Pop :: rest ->
        let (expected_pop, next_model) = pop_model model in
        let actual_pop = Queue.pop queue in
        if not (expected_pop = actual_pop) then
          Error ("step "
          ^ Int.to_string step
          ^ " Pop: expected "
          ^ render_int_option expected_pop
          ^ ", got "
          ^ render_int_option actual_pop)
        else if not (Queue.to_list queue = next_model) then
          Error ("step "
          ^ Int.to_string step
          ^ " Pop: expected queue "
          ^ render_int_list next_model
          ^ ", got "
          ^ render_int_list (Queue.to_list queue))
        else if not (Int.equal (Queue.length queue) (List.length next_model)) then
          Error ("step "
          ^ Int.to_string step
          ^ " Pop: expected length "
          ^ Int.to_string (List.length next_model)
          ^ ", got "
          ^ Int.to_string (Queue.length queue))
        else
          loop (step + 1) next_model rest
    | Clear :: rest ->
        Queue.clear queue;
        if not (Queue.is_empty queue) then
          Error ("step "
          ^ Int.to_string step
          ^ " Clear: expected queue to be empty, got "
          ^ render_int_list (Queue.to_list queue))
        else if not (Int.equal (Queue.length queue) 0) then
          Error ("step "
          ^ Int.to_string step
          ^ " Clear: expected length 0, got "
          ^ Int.to_string (Queue.length queue))
        else
          loop (step + 1) [] rest
  in
  loop 1 [] operations

let test_multiple_producers_preserve_every_value = fun _ctx ->
  let queue = Queue.create () in
  let batches = make_batches [ 25; 25; 25; 25; ] in
  let parent = self () in
  let producer_pids = spawn_producers ~parent ~queue ~batches ~yield_every:5 ~on_done:(fun () -> ()) in
  List.for_each producer_pids ~fn:(fun pid -> send pid Queue_test_go);
  match wait_for_producers ~producer_count:(List.length batches) with
  | Error _ as error -> error
  | Ok () ->
      let drained = drain queue in
      let expected = flatten batches in
      if not (has_expected_set ~expected ~actual:drained) then
        Error "expected Queue to preserve the full produced set under concurrent producers"
      else if not (preserves_per_producer_fifo ~batches ~actual:drained) then
        Error "expected Queue to preserve per-producer FIFO order under concurrent producers"
      else if not (Queue.is_empty queue) then
        Error "expected Queue to be empty after draining concurrent producer output"
      else
        Ok ()

let test_multiple_consumers_drain_every_value_once = fun _ctx ->
  let values = ints_from 0 100 in
  let queue = Queue.from_list values in
  let parent = self () in
  let consumer_count = 4 in
  let rec spawn_consumers remaining acc =
    if remaining = 0 then
      List.reverse acc
    else
      let pid =
        spawn
          (fun () ->
            wait_for_go ();
            let rec consume acc =
              match Queue.pop queue with
              | None ->
                  send parent (Queue_consumer_values (List.reverse acc));
                  Ok ()
              | Some value ->
                  if value mod 7 = 0 then
                    yield ();
                  consume (value :: acc)
            in
            consume [])
      in
      spawn_consumers (remaining - 1) (pid :: acc)
  in
  let consumer_pids = spawn_consumers consumer_count [] in
  List.for_each consumer_pids ~fn:(fun pid -> send pid Queue_test_go);
  match collect_consumer_values ~consumer_count with
  | Error _ as error -> error
  | Ok consumed_lists ->
      let consumed = flatten consumed_lists in
      if not (has_expected_set ~expected:values ~actual:consumed) then
        Error "expected Queue consumers to drain each prefetched value exactly once"
      else if not (Queue.is_empty queue) then
        Error "expected Queue to be empty after concurrent consumers finish"
      else
        Ok ()

let test_mixed_producers_and_consumers_preserve_every_value = fun _ctx ->
  let queue = Queue.create () in
  let batches = make_batches [ 20; 20; 20; 20; ] in
  let parent = self () in
  let producer_count = List.length batches in
  let consumer_count = 4 in
  let done_producers = Sync.Atomic.make 0 in
  let rec spawn_consumers remaining acc =
    if remaining = 0 then
      List.reverse acc
    else
      let pid =
        spawn
          (fun () ->
            wait_for_go ();
            let rec consume acc =
              match Queue.pop queue with
              | Some value ->
                  if value mod 11 = 0 then
                    yield ();
                  consume (value :: acc)
              | None ->
                  if Int.equal (Sync.Atomic.get done_producers) producer_count then
                    (
                      send parent (Queue_consumer_values (List.reverse acc));
                      Ok ()
                    )
                  else
                    (
                      yield ();
                      consume acc
                    )
            in
            consume [])
      in
      spawn_consumers (remaining - 1) (pid :: acc)
  in
  let consumer_pids = spawn_consumers consumer_count [] in
  let producer_pids =
    spawn_producers
      ~parent
      ~queue
      ~batches
      ~yield_every:3
      ~on_done:(fun () -> ignore (Sync.Atomic.fetch_and_add done_producers 1))
  in
  List.for_each consumer_pids ~fn:(fun pid -> send pid Queue_test_go);
  List.for_each producer_pids ~fn:(fun pid -> send pid Queue_test_go);
  match wait_for_producers ~producer_count with
  | Error _ as error -> error
  | Ok () -> (
      match collect_consumer_values ~consumer_count with
      | Error _ as error -> error
      | Ok consumed_lists ->
          let consumed = flatten consumed_lists in
          let expected = flatten batches in
          if not (has_expected_set ~expected ~actual:consumed) then
            Error "expected Queue to deliver each value exactly once with concurrent producers and consumers"
          else if not (Queue.is_empty queue) then
            Error "expected Queue to be empty after mixed concurrent producers and consumers"
          else
            Ok ()
    )

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
    case "Queue handles the clear/pop regression sequence" test_regression_clear_pop_sequence;
    case
      ~size:Large
      "Queue preserves every value from concurrent producers"
      test_multiple_producers_preserve_every_value;
    case
      ~size:Large
      "Queue lets multiple consumers drain each prefetched value exactly once"
      test_multiple_consumers_drain_every_value_once;
    case
      ~size:Large
      "Queue preserves every value with concurrent producers and consumers"
      test_mixed_producers_and_consumers_preserve_every_value;
  ]

let main ~args = Test.Cli.main ~name:"queue" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
