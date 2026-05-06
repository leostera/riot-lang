open Std

module Test = Std.Test
module Graph_scheduler = Riot_build.Internal.Graph_scheduler

type mutation =
  | Note of string

let make_graph = fun ~apply_mutation tasks ->
  let graph = Graph_scheduler.Graph.create ~apply_mutation () in
  let node_ids =
    List.map tasks ~fn:(fun task -> Graph_scheduler.Graph.add_node graph ~payload:task)
  in
  (graph, node_ids)

let result_labels = fun results ->
  List.map
    results.Graph_scheduler.results
    ~fn:(fun (result: (int, string, string) Graph_scheduler.node_result) ->
      match result.outcome with
      | Ok value -> Int.to_string result.payload ^ ":" ^ value
      | Error err -> Int.to_string result.payload ^ ":error:" ^ err)

let run_graph = fun
  ?(parallelism = 1)
  ?(mode = Graph_scheduler.Run_config.Continue_on_failure)
  ?(on_event = fun () -> ())
  ~graph
  ~execute
  () ->
  Graph_scheduler.run
    ~config:(Graph_scheduler.Run_config.make ~parallelism ~mode ())
    ~on_event
    ~graph
    ~execute

let run_tasks = fun
  ?parallelism ?mode ?on_event ?(apply_mutation = fun _ (_:mutation) -> ()) ~tasks ~execute () ->
  let (graph, _) = make_graph ~apply_mutation tasks in
  run_graph ?parallelism ?mode ?on_event ~graph ~execute ()

let test_graph_scheduler_runs_generated_work = fun _ctx ->
  let results =
    run_tasks
      ~tasks:[ 1 ]
      ~execute:(fun ~graph ~node:_ ~payload ->
        match payload with
        | 1 ->
            ignore (Graph_scheduler.Handle.add_node graph ~payload:2);
            ignore (Graph_scheduler.Handle.add_node graph ~payload:3);
            Ok "root"
        | 2 -> Ok "left"
        | 3 -> Ok "right"
        | task -> Error ("unexpected task " ^ Int.to_string task))
      ()
  in
  Test.assert_equal ~expected:[ "1:root"; "2:left"; "3:right" ] ~actual:(result_labels results);
  Ok ()

let test_graph_scheduler_queues_generated_work_without_waiting_for_wave = fun _ctx ->
  let slow_finished = ref false in
  let generated_started_before_slow_finished = ref false in
  let results =
    run_tasks
      ~parallelism:2
      ~tasks:[ 1; 2 ]
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
      ()
  in
  if not !generated_started_before_slow_finished then
    Error "expected generated work to start before slow sibling finished"
  else (
    Test.assert_equal
      ~expected:[ "1:root"; "2:slow"; "3:generated" ]
      ~actual:(result_labels results);
    Ok ()
  )

let test_graph_scheduler_records_errors_and_continues_ready_work = fun _ctx ->
  let results =
    run_tasks
      ~parallelism:2
      ~tasks:[ 1; 2 ]
      ~execute:(fun ~graph ~node:_ ~payload ->
        match payload with
        | 1 -> Error "boom"
        | 2 ->
            ignore (Graph_scheduler.Handle.add_node graph ~payload:3);
            Ok "ok"
        | 3 -> Ok "generated"
        | task -> Error ("unexpected task " ^ Int.to_string task))
      ()
  in
  Test.assert_equal
    ~expected:[ "1:error:boom"; "2:ok"; "3:generated" ]
    ~actual:(result_labels results);
  Ok ()

let test_graph_scheduler_generated_dependencies_gate_execution = fun _ctx ->
  let left_finished = ref false in
  let results =
    run_tasks
      ~parallelism:2
      ~tasks:[ 1 ]
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
      ()
  in
  Test.assert_equal ~expected:[ "1:root"; "2:left"; "3:right" ] ~actual:(result_labels results);
  Ok ()

let test_graph_scheduler_static_dependencies_gate_execution = fun _ctx ->
  let order = ref [] in
  let (graph, node_ids) = make_graph ~apply_mutation:(fun _ (_: mutation) -> ()) [ 1; 2; 3 ] in
  match node_ids with
  | [ one; two; three ] ->
      Graph_scheduler.Graph.add_dependency graph ~node:two ~depends_on:one;
      Graph_scheduler.Graph.add_dependency graph ~node:three ~depends_on:two;
      let results =
        run_graph
          ~parallelism:3
          ~graph
          ~execute:(fun ~graph:_ ~node:_ ~payload ->
            order := !order @ [ payload ];
            Ok ("done-" ^ Int.to_string payload))
          ()
      in
      Test.assert_equal ~expected:[ 1; 2; 3 ] ~actual:!order;
      Test.assert_equal
        ~expected:[ "1:done-1"; "2:done-2"; "3:done-3" ]
        ~actual:(result_labels results);
      Ok ()
  | _ -> Error "expected three graph nodes"

let test_graph_scheduler_fail_fast_stops_new_generated_work = fun _ctx ->
  let generated_ran = ref false in
  let results =
    run_tasks
      ~parallelism:2
      ~mode:Graph_scheduler.Run_config.Fail_fast
      ~tasks:[ 1; 2 ]
      ~execute:(fun ~graph ~node:_ ~payload ->
        match payload with
        | 1 -> Error "boom"
        | 2 ->
            sleep (Time.Duration.from_millis 50);
            ignore (Graph_scheduler.Handle.add_node graph ~payload:3);
            Ok "slow"
        | 3 ->
            generated_ran := true;
            Ok "generated"
        | task -> Error ("unexpected task " ^ Int.to_string task))
      ()
  in
  if !generated_ran then
    Error "expected fail-fast mode to block newly generated work after a failure"
  else (
    Test.assert_equal ~expected:[ "1:error:boom"; "2:slow" ] ~actual:(result_labels results);
    Ok ()
  )

let test_graph_scheduler_emits_events_in_completion_order = fun _ctx ->
  let events = ref [] in
  let (graph, node_ids) = make_graph ~apply_mutation:(fun _ (_: mutation) -> ()) [ 1; 2 ] in
  match node_ids with
  | [ one; two ] ->
      Graph_scheduler.Graph.add_dependency graph ~node:two ~depends_on:one;
      let _ =
        Graph_scheduler.run
          ~config:(Graph_scheduler.Run_config.make
            ~parallelism:1
            ~mode:Graph_scheduler.Run_config.Continue_on_failure
            ())
          ~graph
          ~on_event:(fun event -> events := !events @ [ event ])
          ~execute:(fun ~graph ~node:_ ~payload ->
            Graph_scheduler.Handle.emit_event graph ("node-" ^ Int.to_string payload);
            Ok "ok")
      in
      Test.assert_equal ~expected:[ "node-1"; "node-2" ] ~actual:!events;
      Ok ()
  | _ -> Error "expected two graph nodes"

let test_graph_scheduler_emits_events_before_node_completion = fun _ctx ->
  let events = ref [] in
  let event_seen_before_completion = ref false in
  let (graph, _) = make_graph ~apply_mutation:(fun _ (_: mutation) -> ()) [ 1 ] in
  let _ =
    Graph_scheduler.run
      ~config:(Graph_scheduler.Run_config.make
        ~parallelism:1
        ~mode:Graph_scheduler.Run_config.Continue_on_failure
        ())
      ~graph
      ~on_event:(fun event -> events := !events @ [ event ])
      ~execute:(fun ~graph ~node:_ ~payload ->
        Graph_scheduler.Handle.emit_event graph ("node-" ^ Int.to_string payload);
        sleep (Time.Duration.from_millis 80);
        event_seen_before_completion := (
          match !events with
          | [ event ] -> String.equal event "node-1"
          | _ -> false
        );
        Ok "ok")
  in
  if !event_seen_before_completion then
    Ok ()
  else
    Error "expected event callback to run before the node completed"

let test_graph_scheduler_applies_recorded_mutations = fun _ctx ->
  let applied = ref [] in
  let results =
    run_tasks
      ~tasks:[ 1; 2 ]
      ~apply_mutation:(fun _ mutation ->
        match mutation with
        | Note note -> applied := !applied @ [ note ])
      ~execute:(fun ~graph ~node:_ ~payload ->
        Graph_scheduler.Handle.record graph (Note ("payload-" ^ Int.to_string payload));
        Ok ("ok-" ^ Int.to_string payload))
      ()
  in
  Test.assert_equal ~expected:[ "payload-1"; "payload-2" ] ~actual:!applied;
  Test.assert_equal ~expected:[ "1:ok-1"; "2:ok-2" ] ~actual:(result_labels results);
  Ok ()

let test_graph_scheduler_empty_graph_returns_empty_results = fun _ctx ->
  let graph = Graph_scheduler.Graph.create ~apply_mutation:(fun _ (_: mutation) -> ()) () in
  let results = run_graph ~graph ~execute:(fun ~graph:_ ~node:_ ~payload:_ -> Ok "unused") () in
  Test.assert_equal ~expected:[] ~actual:(result_labels results);
  Ok ()

let test_graph_scheduler_completes_gate_nodes_without_executing_them = fun _ctx ->
  let executed = ref [] in
  let (graph, node_ids) = make_graph ~apply_mutation:(fun _ (_: mutation) -> ()) [ 1; 2; 3 ] in
  match node_ids with
  | [ one; gate; dependent ] ->
      Graph_scheduler.Graph.add_dependency graph ~node:gate ~depends_on:one;
      Graph_scheduler.Graph.add_dependency graph ~node:dependent ~depends_on:gate;
      let results =
        run_graph
          ~parallelism:2
          ~graph
          ~execute:(fun ~graph ~node:_ ~payload ->
            executed := payload :: !executed;
            match payload with
            | 1 ->
                Graph_scheduler.Handle.complete_node graph ~node:gate ~outcome:(Ok "cached-gate");
                Ok "root"
            | 2 -> Error "gate should have been completed without execution"
            | 3 -> Ok "dependent"
            | task -> Error ("unexpected task " ^ Int.to_string task))
          ()
      in
      Test.assert_equal ~expected:[ 3; 1 ] ~actual:!executed;
      Test.assert_equal
        ~expected:[ "1:root"; "2:cached-gate"; "3:dependent" ]
        ~actual:(result_labels results);
      Ok ()
  | _ -> Error "expected three graph nodes"

let test_graph_scheduler_ignores_workers_from_previous_runs = fun _ctx ->
  let rec loop run_index =
    if run_index = 20 then
      Ok ()
    else
      let results =
        run_tasks
          ~parallelism:4
          ~tasks:[ 1; 2 ]
          ~execute:(fun ~graph:_ ~node:_ ~payload:task ->
            Ok ("run-" ^ Int.to_string run_index ^ "-" ^ Int.to_string task))
          ()
      in
      match result_labels results with
      | [ _; _ ] -> loop (run_index + 1)
      | labels ->
          Error ("expected each repeated scheduler run to complete two tasks, got ["
          ^ String.concat ", " labels
          ^ "]")
  in
  loop 0

let tests = let open Test in
[
  case "graph scheduler: runs generated work" test_graph_scheduler_runs_generated_work;
  case
    "graph scheduler: generated work does not wait for sibling wave"
    test_graph_scheduler_queues_generated_work_without_waiting_for_wave;
  case
    "graph scheduler: records errors and continues ready work"
    test_graph_scheduler_records_errors_and_continues_ready_work;
  case
    "graph scheduler: generated dependencies gate execution"
    test_graph_scheduler_generated_dependencies_gate_execution;
  case
    "graph scheduler: static dependencies gate execution"
    test_graph_scheduler_static_dependencies_gate_execution;
  case
    "graph scheduler: fail-fast blocks new generated work"
    test_graph_scheduler_fail_fast_stops_new_generated_work;
  case
    "graph scheduler: emits events in completion order"
    test_graph_scheduler_emits_events_in_completion_order;
  case
    "graph scheduler: emits events before node completion"
    test_graph_scheduler_emits_events_before_node_completion;
  case "graph scheduler: applies recorded mutations" test_graph_scheduler_applies_recorded_mutations;
  case
    "graph scheduler: empty graphs return no results"
    test_graph_scheduler_empty_graph_returns_empty_results;
  case
    "graph scheduler: completes gate nodes without executing them"
    test_graph_scheduler_completes_gate_nodes_without_executing_them;
  case
    "graph scheduler: ignores ready workers from previous runs"
    test_graph_scheduler_ignores_workers_from_previous_runs;
]

let name = "Riot Graph Scheduler Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
