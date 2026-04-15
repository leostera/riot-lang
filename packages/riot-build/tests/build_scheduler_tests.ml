open Std

module Test = Std.Test
module Build_scheduler = Riot_build.Internal.Build_scheduler

let result_labels = fun results ->
  List.map results
    ~fn:(fun (task, outcome) ->
      match outcome with
      | Ok value -> Int.to_string task ^ ":" ^ value
      | Error err -> Int.to_string task ^ ":error:" ^ err)

let test_scheduler_runs_generated_work = fun _ctx ->
  let results =
    Build_scheduler.run
      ~concurrency:1
      ~tasks:[ 1 ]
      ~fn:(function
        | 1 -> Ok ("root", [ 2; 3 ])
        | 2 -> Ok ("left", [])
        | 3 -> Ok ("right", [])
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
    Build_scheduler.run
      ~concurrency:2
      ~tasks:[ 1; 2 ]
      ~fn:(function
        | 1 -> Ok ("root", [ 3 ])
        | 2 ->
            sleep (Time.Duration.from_millis 80);
            slow_finished := true;
            Ok ("slow", [])
        | 3 ->
            generated_started_before_slow_finished := not !slow_finished;
            Ok ("generated", [])
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
    Build_scheduler.run
      ~concurrency:2
      ~tasks:[ 1; 2 ]
      ~fn:(function
        | 1 -> Error "boom"
        | 2 -> Ok ("ok", [ 3 ])
        | 3 -> Ok ("generated", [])
        | task -> Error ("unexpected task " ^ Int.to_string task))
  in
  Test.assert_equal
    ~expected:[ "1:error:boom"; "2:ok"; "3:generated" ]
    ~actual:(result_labels results);
  Ok ()

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
  ]

let name = "Riot Build Scheduler Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
