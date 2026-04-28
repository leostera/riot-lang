open Std
open Propane

module Test = Std.Test
module Queue = Collections.Queue

type Message.t +=
  | Queue_property_go
  | Queue_property_producer_done of int
  | Queue_property_consumer_values of int list

type operation =
  | Push of int
  | Pop
  | Clear

let sequential_examples = 500

let concurrent_examples = 100

let assert_property = fun _ctx ~examples name property ->
  let config = { Property.default_config with test_count = examples } in
  match Property.check ~config ~on_progress:(Test.Context.emit_progress _ctx) property with
  | Property.Success -> Ok ()
  | Property.Failure { counter_example; shrink_steps } ->
      Error (name
      ^ " failed\nCounter-example:\n"
      ^ counter_example
      ^ "\nShrink steps: "
      ^ Int.to_string shrink_steps)
  | Property.Error { exception_ = _; backtrace } ->
      Error (name ^ " raised an unexpected exception\n" ^ backtrace)
  | Property.Assumption_violated -> Error (name ^ " exhausted assumptions")

let print_operation = function
  | Push value -> "Push(" ^ Int.to_string value ^ ")"
  | Pop -> "Pop"
  | Clear -> "Clear"

let operation_gen =
  Generator.frequency
    [
      (5, Generator.map (fun value -> Push value) (Generator.int_range (-32) 32));
      (3, Generator.return Pop);
      (1, Generator.return Clear);
    ]

let operation_arb =
  Arbitrary.make
    ~print:(Printer.list print_operation)
    (Generator.list_size (Generator.int_range 0 64) operation_gen)

let bounded_int_arb = Arbitrary.map_gen (Generator.int_range 0 24) Arbitrary.int

let producer_sizes_arb =
  Arbitrary.map_gen
    (Generator.list_size (Generator.int_range 1 6) bounded_int_arb.gen)
    (Arbitrary.list bounded_int_arb)

let consumer_count_arb = Arbitrary.map_gen (Generator.int_range 1 6) Arbitrary.int

let item_count_arb = Arbitrary.map_gen (Generator.int_range 0 120) Arbitrary.int

let consumer_scenario_arb = Arbitrary.pair consumer_count_arb item_count_arb

let mixed_scenario_arb = Arbitrary.pair producer_sizes_arb consumer_count_arb

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 2) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let wait_for_go = fun () ->
  let _ =
    receive
      ~selector:(
        function
        | Queue_property_go -> Select ()
        | _ -> Skip
      )
      ~timeout:(Time.Duration.from_secs 2)
      ()
  in
  ()

let rec ints_from start count =
  if count <= 0 then
    []
  else
    start :: ints_from (start + 1) (count - 1)

let rec flatten lists =
  match lists with
  | [] -> []
  | values :: rest -> values @ flatten rest

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let render_int_list = fun values ->
  "[" ^ String.concat ", " (List.map values ~fn:Int.to_string) ^ "]"

let render_int_option = function
  | None -> "None"
  | Some value -> "Some(" ^ Int.to_string value ^ ")"

let rec pop_model = function
  | [] -> (None, [])
  | value :: rest -> (Some value, rest)

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
              send parent (Queue_property_producer_done producer);
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
        ~what:"queue property producer completion"
        (
          function
          | Queue_property_producer_done _ -> Select ()
          | _ -> Skip
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
        ~what:"queue property consumer completion"
        (
          function
          | Queue_property_consumer_values values -> Select values
          | _ -> Skip
        ) with
      | Ok values -> loop (remaining - 1) (values :: acc)
      | Error _ as error -> error
  in
  loop consumer_count []

let drain = fun queue ->
  let rec loop acc =
    match Queue.pop queue with
    | None -> List.reverse acc
    | Some value -> loop (value :: acc)
  in
  loop []

let preserves_per_producer_fifo = fun ~batches ~actual ->
  let rec loop producer remaining_batches =
    match remaining_batches with
    | [] -> true
    | batch :: rest ->
        let observed = List.filter actual ~fn:(fun value -> value / 1_000_000 = producer) in
        batch = observed && loop (producer + 1) rest
  in
  loop 0 batches

let sequential_operations_match_model =
  Property.for_all
    operation_arb
    (fun operations ->
      let queue = Queue.create () in
      let rec loop step model remaining =
        match remaining with
        | [] ->
            if not (Queue.to_list queue = model) then
              Property.fail
                ("final state: expected queue "
                ^ render_int_list model
                ^ ", got "
                ^ render_int_list (Queue.to_list queue))
            else if not (Int.equal (Queue.length queue) (List.length model)) then
              Property.fail
                ("final state: expected length "
                ^ Int.to_string (List.length model)
                ^ ", got "
                ^ Int.to_string (Queue.length queue))
            else if not (Bool.equal (Queue.is_empty queue) (List.is_empty model)) then
              Property.fail
                ("final state: expected is_empty="
                ^ Bool.to_string (List.is_empty model)
                ^ ", got "
                ^ Bool.to_string (Queue.is_empty queue))
            else
              true
        | (Push value) :: rest ->
            Queue.push queue ~value;
            let next_model = model @ [ value ] in
            if not (Queue.to_list queue = next_model) then
              Property.fail
                ("step "
                ^ Int.to_string step
                ^ " Push("
                ^ Int.to_string value
                ^ "): expected queue "
                ^ render_int_list next_model
                ^ ", got "
                ^ render_int_list (Queue.to_list queue))
            else if not (Int.equal (Queue.length queue) (List.length next_model)) then
              Property.fail
                ("step "
                ^ Int.to_string step
                ^ " Push("
                ^ Int.to_string value
                ^ "): expected length "
                ^ Int.to_string (List.length next_model)
                ^ ", got "
                ^ Int.to_string (Queue.length queue))
            else
              loop (step + 1) next_model rest
        | Pop :: rest ->
            let (expected_pop, next_model) = pop_model model in
            let actual_pop = Queue.pop queue in
            if not (expected_pop = actual_pop) then
              Property.fail
                ("step "
                ^ Int.to_string step
                ^ " Pop: expected "
                ^ render_int_option expected_pop
                ^ ", got "
                ^ render_int_option actual_pop)
            else if not (Queue.to_list queue = next_model) then
              Property.fail
                ("step "
                ^ Int.to_string step
                ^ " Pop: expected queue "
                ^ render_int_list next_model
                ^ ", got "
                ^ render_int_list (Queue.to_list queue))
            else if not (Int.equal (Queue.length queue) (List.length next_model)) then
              Property.fail
                ("step "
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
              Property.fail
                ("step "
                ^ Int.to_string step
                ^ " Clear: expected queue to be empty, got "
                ^ render_int_list (Queue.to_list queue))
            else if not (Int.equal (Queue.length queue) 0) then
              Property.fail
                ("step "
                ^ Int.to_string step
                ^ " Clear: expected length 0, got "
                ^ Int.to_string (Queue.length queue))
            else
              loop (step + 1) [] rest
      in
      loop 1 [] operations)

let concurrent_producers_preserve_every_value =
  Property.for_all
    producer_sizes_arb
    (fun sizes ->
      let queue = Queue.create () in
      let batches = make_batches sizes in
      let parent = self () in
      let producer_pids =
        spawn_producers ~parent ~queue ~batches ~yield_every:5 ~on_done:(fun () -> ())
      in
      List.for_each producer_pids ~fn:(fun pid -> send pid Queue_property_go);
      match wait_for_producers ~producer_count:(List.length batches) with
      | Error error -> Property.fail ("producer coordination failed: " ^ error)
      | Ok () ->
          let drained = drain queue in
          let expected = flatten batches in
          if not (sort_ints drained = sort_ints expected) then
            Property.fail
              ("expected produced set "
              ^ render_int_list (sort_ints expected)
              ^ ", got "
              ^ render_int_list (sort_ints drained))
          else if not (preserves_per_producer_fifo ~batches ~actual:drained) then
            Property.fail ("expected per-producer FIFO order, got " ^ render_int_list drained)
          else if not (Queue.is_empty queue) then
            Property.fail
              ("expected queue to be empty after draining, got "
              ^ render_int_list (Queue.to_list queue))
          else
            true)

let concurrent_consumers_drain_each_value_once =
  Property.for_all
    consumer_scenario_arb
    (fun (consumer_count, item_count) ->
      let values = ints_from 0 item_count in
      let queue = Queue.from_list values in
      let parent = self () in
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
                      send parent (Queue_property_consumer_values (List.reverse acc));
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
      List.for_each consumer_pids ~fn:(fun pid -> send pid Queue_property_go);
      match collect_consumer_values ~consumer_count with
      | Error error -> Property.fail ("consumer coordination failed: " ^ error)
      | Ok consumed_lists ->
          let consumed = flatten consumed_lists in
          if not (sort_ints consumed = sort_ints values) then
            Property.fail
              ("expected consumed set "
              ^ render_int_list (sort_ints values)
              ^ ", got "
              ^ render_int_list (sort_ints consumed))
          else if not (Queue.is_empty queue) then
            Property.fail
              ("expected queue to be empty after consumers finish, got "
              ^ render_int_list (Queue.to_list queue))
          else
            true)

let concurrent_producers_and_consumers_preserve_every_value =
  Property.for_all
    mixed_scenario_arb
    (fun (sizes, consumer_count) ->
      let queue = Queue.create () in
      let batches = make_batches sizes in
      let expected = flatten batches in
      let parent = self () in
      let producer_count = List.length batches in
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
                      if Int.equal (Sync.Atomic.get done_producers) producer_count then (
                        send parent (Queue_property_consumer_values (List.reverse acc));
                        Ok ()
                      ) else (
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
      List.for_each consumer_pids ~fn:(fun pid -> send pid Queue_property_go);
      List.for_each producer_pids ~fn:(fun pid -> send pid Queue_property_go);
      match wait_for_producers ~producer_count with
      | Error error ->
          Property.fail ("mixed coordination failed while waiting for producers: " ^ error)
      | Ok () ->
          match collect_consumer_values ~consumer_count with
          | Error error ->
              Property.fail ("mixed coordination failed while waiting for consumers: " ^ error)
          | Ok consumed_lists ->
              let consumed = flatten consumed_lists in
              if not (sort_ints consumed = sort_ints expected) then
                Property.fail
                  ("expected mixed produced/consumed set "
                  ^ render_int_list (sort_ints expected)
                  ^ ", got "
                  ^ render_int_list (sort_ints consumed))
              else if not (Queue.is_empty queue) then
                Property.fail
                  ("expected queue to be empty after mixed run, got "
                  ^ render_int_list (Queue.to_list queue))
              else
                true)

let tests = [
  Test.property
    "Queue sequential operations match the reference model"
    ~examples:sequential_examples
    (fun ctx ->
      assert_property
        ctx
        ~examples:sequential_examples
        "Queue sequential operations match the reference model"
        sequential_operations_match_model);
  Test.property
    ~size:Test.Large
    "Queue concurrent producers preserve every value and per-producer FIFO order"
    ~examples:concurrent_examples
    (fun ctx ->
      assert_property
        ctx
        ~examples:concurrent_examples
        "Queue concurrent producers preserve every value and per-producer FIFO order"
        concurrent_producers_preserve_every_value);
  Test.property
    ~size:Test.Large
    "Queue concurrent consumers drain each prefetched value exactly once"
    ~examples:concurrent_examples
    (fun ctx ->
      assert_property
        ctx
        ~examples:concurrent_examples
        "Queue concurrent consumers drain each prefetched value exactly once"
        concurrent_consumers_drain_each_value_once);
  Test.property
    ~size:Test.Large
    "Queue concurrent producers and consumers preserve every value"
    ~examples:concurrent_examples
    (fun ctx ->
      assert_property
        ctx
        ~examples:concurrent_examples
        "Queue concurrent producers and consumers preserve every value"
        concurrent_producers_and_consumers_preserve_every_value);
]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"queue_property" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
