open Std

module Test = Std.Test
module Graph_scheduler = Riot_build.Internal.Graph_scheduler

let result_labels = fun results ->
  List.map results.Graph_scheduler.results
    ~fn:(fun (result: (int, string, string) Graph_scheduler.node_result) ->
      match result.outcome with
      | Ok value -> Int.to_string result.payload ^ ":" ^ value
      | Error err -> Int.to_string result.payload ^ ":error:" ^ err)

let make_graph = fun tasks ->
  let graph = Graph_scheduler.Graph.create ~apply_mutation:(fun _ () -> ()) () in
  List.for_each tasks ~fn:(fun task ->
    ignore (Graph_scheduler.Graph.add_node graph ~payload:task));
  graph

let run = fun ~tasks ~parallelism ~execute ->
  Graph_scheduler.run
    ~config:(Graph_scheduler.Run_config.make
      ~parallelism
      ~mode:Graph_scheduler.Run_config.Continue_on_failure
      ())
    ~on_event:(fun () -> ())
    ~graph:(make_graph tasks)
    ~execute

let test_scheduler_runs_generated_work = fun _ctx ->
  let results =
    run
      ~tasks:[ 1 ]
      ~parallelism:1
      ~execute:(fun ~graph ~node:_ ~payload ->
        match payload with
        | 1 ->
            ignore (Graph_scheduler.Handle.add_node graph ~payload:2);
            ignore (Graph_scheduler.Handle.add_node graph ~payload:3);
            Ok "root"
        | 2 -> Ok "left"
        | 3 -> Ok "right"
        | task -> Error ("unexpected task " ^ Int.to_string task))
  in
  Test.assert_equal
    ~expected:[ "1:root"; "2:left"; "3:right" ]
    ~actual:(result_labels results);
  Ok ()

let test_scheduler_queues_generated_work_without_waiting_for_wave = fun _ctx ->
  let slow_finished = ref false in
  let generated_started_before_slow_finished = ref false in
  let results =
    run
      ~tasks:[ 1; 2 ]
      ~parallelism:2
      ~execute:(fun ~graph ~node:_ ~payload ->
        match payload with
        | 1 ->
            ignore (Graph_scheduler.Handle.add_node graph ~payload:3);
            Ok "root"
        | 2 ->
            sleep (Time.Duration.from_millis 80);
            slow_finished := true;
            Ok "slow"
        | 3 ->
            generated_started_before_slow_finished := not !slow_finished;
            Ok "generated"
        | task -> Error ("unexpected task " ^ Int.to_string task))
  in
  if not !generated_started_before_slow_finished then
    Error "expected generated work to start before slow sibling finished"
  else (
    Test.assert_equal
      ~expected:[ "1:root"; "2:slow"; "3:generated" ]
      ~actual:(result_labels results);
    Ok ()
  )

let test_scheduler_records_errors_and_continues_ready_work = fun _ctx ->
  let results =
    run
      ~tasks:[ 1; 2 ]
      ~parallelism:2
      ~execute:(fun ~graph ~node:_ ~payload ->
        match payload with
        | 1 -> Error "boom"
        | 2 ->
            ignore (Graph_scheduler.Handle.add_node graph ~payload:3);
            Ok "ok"
        | 3 -> Ok "generated"
        | task -> Error ("unexpected task " ^ Int.to_string task))
  in
  Test.assert_equal
    ~expected:[ "1:error:boom"; "2:ok"; "3:generated" ]
    ~actual:(result_labels results);
  Ok ()

let test_scheduler_honors_generated_dependencies = fun _ctx ->
  let left_finished = ref false in
  let results =
    run
      ~tasks:[ 1 ]
      ~parallelism:2
      ~execute:(fun ~graph ~node:_ ~payload ->
        match payload with
        | 1 ->
            let left = Graph_scheduler.Handle.add_node graph ~payload:2 in
            let right = Graph_scheduler.Handle.add_node graph ~payload:3 in
            Graph_scheduler.Handle.add_dependency graph ~node:right ~depends_on:left;
            Ok "root"
        | 2 ->
            left_finished := true;
            Ok "left"
        | 3 ->
            if not !left_finished then
              Error "expected generated dependency to run first"
            else
              Ok "right"
        | task -> Error ("unexpected task " ^ Int.to_string task))
  in
  Test.assert_equal
    ~expected:[ "1:root"; "2:left"; "3:right" ]
    ~actual:(result_labels results);
  Ok ()

let test_scheduler_ignores_workers_from_previous_runs = fun _ctx ->
  let rec loop run_index =
    if run_index = 20 then
      Ok ()
    else
      let results =
        run
          ~tasks:[ 1; 2 ]
          ~parallelism:4
          ~execute:(fun ~graph:_ ~node:_ ~payload:task ->
            Ok ("run-" ^ Int.to_string run_index ^ "-" ^ Int.to_string task))
      in
      match result_labels results with
      | [ _; _ ] -> loop (run_index + 1)
      | labels ->
          Error
            ("expected each repeated scheduler run to complete two tasks, got ["
            ^ String.concat ", " labels
            ^ "]")
  in
  loop 0

let tests =
  let open Test in
  [
    case "build scheduler: runs generated work" test_scheduler_runs_generated_work;
    case
      "build scheduler: generated work does not wait for sibling wave"
      test_scheduler_queues_generated_work_without_waiting_for_wave;
    case
      "build scheduler: records errors and continues ready work"
      test_scheduler_records_errors_and_continues_ready_work;
    case
      "build scheduler: generated dependencies gate execution"
      test_scheduler_honors_generated_dependencies;
    case
      "build scheduler: ignores ready workers from previous runs"
      test_scheduler_ignores_workers_from_previous_runs;
  ]

let name = "Riot Build Scheduler Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
