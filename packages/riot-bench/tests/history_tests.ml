open Std

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let read_json = fun path ->
  let content =
    Fs.read_to_string path
    |> Result.expect ~msg:"failed to read written history file"
  in
  Data.Json.from_string content
  |> Result.expect ~msg:"failed to parse written history json"

let field = fun name fields ->
  List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name)
  |> Option.map ~fn:(fun (_, value) -> value)

let sample_stats = fun nanos ->
  {
    Riot_bench.History.min = Time.Duration.from_nanos nanos;
    max = Time.Duration.from_nanos (nanos + 10);
    mean = Time.Duration.from_nanos (nanos + 20);
    median = Time.Duration.from_nanos (nanos + 30);
    std_dev = Time.Duration.from_nanos 5;
    iterations = 100;
    total_time = Time.Duration.from_nanos ((nanos + 20) * 100);
    gc = { minor_collections = 1; major_collections = 0; compactions = 0 };
  }

let stats = fun
  ?(minor_collections = 1)
  ?(major_collections = 0)
  ?(compactions = 0)
  ~min
  ~max
  ~mean
  ~median
  ~std_dev
  ~iterations
  ~total_time
  () ->
  {
    Riot_bench.History.min = Time.Duration.from_nanos min;
    max = Time.Duration.from_nanos max;
    mean = Time.Duration.from_nanos mean;
    median = Time.Duration.from_nanos median;
    std_dev = Time.Duration.from_nanos std_dev;
    iterations;
    total_time = Time.Duration.from_nanos total_time;
    gc = { minor_collections; major_collections; compactions };
  }

let benchmark = fun ~index ~name nanos -> {
  Riot_bench.History.index;
  name;
  result = Completed (sample_stats nanos);
}

let benchmark_with_stats = fun ~index ~name statistics -> {
  Riot_bench.History.index;
  name;
  result = Completed statistics;
}

let suite_run = fun benchmarks comparisons ->
  {
    Riot_bench.History.status = 0;
    started_at_us = Some 10;
    completed_at_us = Some 20;
    duration_us = Some 10;
    summary =
      {
        total = List.length benchmarks + List.length comparisons;
        completed = List.length benchmarks + List.length comparisons;
        skipped = 0;
        failed = 0;
      };
    benchmarks;
    comparisons;
  }

let write_run_with_name = fun ~root ~profile ~run_name ~suite_name ~suite_run ->
  let context =
    Riot_bench.History.create_run_context
      ~workspace_root:root
      ~profile
      ~filter:None
      ~partial:false
      ~argv:[ "riot"; "bench"; "-p"; "serde-json"; ]
      ()
  in
  let saved =
    Riot_bench.History.save_suite_run
      context
      ~package_name:(package_name "serde-json")
      ~suite_name
      ~suite_run
    |> Result.expect ~msg:"failed to save suite history"
    |> Option.expect ~msg:"expected non-empty suite history to be saved"
  in
  let parent_dir =
    Path.parent saved
    |> Option.expect ~msg:"expected suite history path parent"
  in
  let renamed = Path.(parent_dir / Path.v (run_name ^ ".json")) in
  Fs.rename ~src:saved ~dst:renamed
  |> Result.expect ~msg:"failed to rename saved history file";
  renamed

let test_suite_run_path_uses_package_suite_and_run_id = fun _ctx ->
  with_tempdir
    "riot_bench_history_path"
    (fun root ->
      let context =
        Riot_bench.History.create_run_context
          ~workspace_root:root
          ~profile:"debug"
          ~filter:(Some "ioslice")
          ~partial:true
          ~argv:[ "riot"; "bench"; "-p"; "std"; "-f"; "ioslice"; ]
          ()
      in
      let path =
        Riot_bench.History.suite_run_path
          context
          ~package_name:(package_name "std")
          ~suite_name:"std_io_ioslice_bench"
      in
      let rendered = Path.to_string path in
      let run_id = Riot_bench.History.run_id context in
      if not (String.contains rendered ".riot/bench/std/std_io_ioslice_bench/runs/") then
        Error ("unexpected bench history path: " ^ rendered)
      else if not (String.ends_with ~suffix:(run_id ^ ".json") rendered) then
        Error ("expected bench history path to end with run id, got: " ^ rendered)
      else
        Ok ())

let test_save_suite_run_writes_self_contained_json = fun _ctx ->
  with_tempdir
    "riot_bench_history_write"
    (fun root ->
      let context =
        Riot_bench.History.create_run_context
          ~workspace_root:root
          ~profile:"release"
          ~filter:(Some "parser")
          ~partial:true
          ~argv:[ "riot"; "bench"; "-p"; "http"; "-f"; "parser"; "--release"; ]
          ()
      in
      let suite_run: Riot_bench.History.suite_run = {
        status = 0;
        started_at_us = Some 12;
        completed_at_us = Some 34;
        duration_us = Some 22;
        summary =
          {
            total = 1;
            completed = 1;
            skipped = 0;
            failed = 0;
          };
        benchmarks =
          [
            {
              index = 1;
              name = "small request";
              result =
                Completed {
                  min = Time.Duration.from_nanos 10;
                  max = Time.Duration.from_nanos 20;
                  mean = Time.Duration.from_nanos 15;
                  median = Time.Duration.from_nanos 15;
                  std_dev = Time.Duration.from_nanos 3;
                  iterations = 100;
                  total_time = Time.Duration.from_nanos 1_500;
                  gc = { minor_collections = 1; major_collections = 0; compactions = 0 };
                };
            };
          ];
        comparisons = [];
      }
      in
      let path =
        Riot_bench.History.save_suite_run
          context
          ~package_name:(package_name "http")
          ~suite_name:"http1_parser_bench"
          ~suite_run
        |> Result.expect ~msg:"failed to save suite history"
        |> Option.expect ~msg:"expected non-empty suite history to be saved"
      in
      let json = read_json path in
      match json with
      | Data.Json.Object fields -> (
          match (
            field "schema_version" fields,
            field "run_id" fields,
            field "suite" fields,
            field "selection" fields,
            field "suite_run" fields
          ) with
          | (
              Some (Data.Json.Int 1),
              Some (Data.Json.String _),
              Some (Data.Json.Object suite_fields),
              Some (Data.Json.Object selection_fields),
              Some (Data.Json.Object suite_run_fields)
            ) ->
              let suite_name_ok =
                field "name" suite_fields = Some (Data.Json.String "http1_parser_bench")
              in
              let package_ok = field "package" suite_fields = Some (Data.Json.String "http") in
              let profile_ok = field "profile" suite_fields = Some (Data.Json.String "release") in
              let partial_ok = field "partial" selection_fields = Some (Data.Json.Bool true) in
              let filter_ok = field "filter" selection_fields = Some (Data.Json.String "parser") in
              let benchmarks_ok =
                match field "benchmarks" suite_run_fields with
                | Some (Data.Json.Array [ Data.Json.Object benchmark_fields ]) ->
                    field "name" benchmark_fields = Some (Data.Json.String "small request")
                | _ -> false
              in
              if
                suite_name_ok
                && package_ok
                && profile_ok
                && partial_ok
                && filter_ok
                && benchmarks_ok
              then
                Ok ()
              else
                Error "expected saved bench history json to contain suite metadata and benchmark results"
          | _ ->
              Error "expected saved bench history json to expose the top-level schema, suite, selection, and suite_run fields"
        )
      | _ -> Error "expected saved bench history file to be a json object")

let test_save_suite_run_skips_empty_suite = fun _ctx ->
  with_tempdir
    "riot_bench_history_empty"
    (fun root ->
      let context =
        Riot_bench.History.create_run_context
          ~workspace_root:root
          ~profile:"debug"
          ~filter:(Some "ioslice")
          ~partial:true
          ~argv:[ "riot"; "bench"; "-p"; "std"; "-f"; "ioslice"; ]
          ()
      in
      let suite_run: Riot_bench.History.suite_run = {
        status = 0;
        started_at_us = Some 10;
        completed_at_us = Some 20;
        duration_us = Some 10;
        summary =
          {
            total = 0;
            completed = 0;
            skipped = 0;
            failed = 0;
          };
        benchmarks = [];
        comparisons = [];
      }
      in
      let expected_path =
        Riot_bench.History.suite_run_path
          context
          ~package_name:(package_name "std")
          ~suite_name:"std_io_ioslice_bench"
      in
      match Riot_bench.History.save_suite_run
        context
        ~package_name:(package_name "std")
        ~suite_name:"std_io_ioslice_bench"
        ~suite_run with
      | Error error ->
          Error ("expected empty suite history save to be skipped, got error: " ^ error)
      | Ok (Some path) ->
          Error ("expected empty suite history save to skip writing, got: " ^ Path.to_string path)
      | Ok None ->
          match Fs.exists expected_path with
          | Error err -> Error ("expected fs exists check to succeed: " ^ IO.error_message err)
          | Ok true -> Error "expected empty suite history save to leave no file behind"
          | Ok false -> Ok ())

let test_load_recent_suite_runs_filters_and_orders = fun _ctx ->
  with_tempdir
    "riot_bench_history_load"
    (fun root ->
      let suite_name = "large_json_bench" in
      let _ =
        write_run_with_name
          ~root
          ~profile:"debug"
          ~run_name:"2026-04-21T10-00-00.000Z-old"
          ~suite_name
          ~suite_run:(suite_run [ benchmark ~index:1 ~name:"serde decode total (1MB)" 100 ] [])
      in
      let _ =
        write_run_with_name
          ~root
          ~profile:"release"
          ~run_name:"2026-04-21T11-00-00.000Z-release"
          ~suite_name
          ~suite_run:(suite_run [ benchmark ~index:1 ~name:"serde decode total (1MB)" 200 ] [])
      in
      let _ =
        write_run_with_name
          ~root
          ~profile:"debug"
          ~run_name:"2026-04-21T12-00-00.000Z-new"
          ~suite_name
          ~suite_run:(suite_run [ benchmark ~index:1 ~name:"serde decode total (1MB)" 300 ] [])
      in
      let context =
        Riot_bench.History.create_run_context
          ~workspace_root:root
          ~profile:"debug"
          ~filter:(Some "serde decode")
          ~partial:true
          ~argv:[ "riot"; "bench"; "-p"; "serde-json"; "-f"; "serde decode"; "--compare"; "2"; ]
          ()
      in
      let loaded =
        Riot_bench.History.load_recent_suite_runs
          context
          ~package_name:(package_name "serde-json")
          ~suite_name
          ~limit:2
        |> Result.expect ~msg:"failed to load recent suite runs"
      in
      match loaded with
      | [ newest; older ] ->
          Test.assert_equal ~expected:"debug" ~actual:newest.profile;
          Test.assert_equal ~expected:"debug" ~actual:older.profile;
          let newest_mean =
            match newest.suite_run.benchmarks with
            | [ { result = Completed stats; _ } ] -> Time.Duration.to_nanos stats.mean
            | _ -> Int64.zero
          in
          let older_mean =
            match older.suite_run.benchmarks with
            | [ { result = Completed stats; _ } ] -> Time.Duration.to_nanos stats.mean
            | _ -> Int64.zero
          in
          if newest_mean > older_mean then
            Ok ()
          else
            Error "expected newer debug history run to sort before older debug run"
      | _ -> Error ("expected two debug history runs, got " ^ Int.to_string (List.length loaded)))

let test_compare_suite_run_aligns_case_history = fun _ctx ->
  with_tempdir
    "riot_bench_history_compare"
    (fun root ->
      let suite_name = "large_json_bench" in
      let _ =
        write_run_with_name
          ~root
          ~profile:"debug"
          ~run_name:"2026-04-21T10-00-00.000Z-first"
          ~suite_name
          ~suite_run:(suite_run
            [
              benchmark_with_stats
                ~index:1
                ~name:"manual decode from parsed tree (1MB)"
                (stats
                  ~minor_collections:4
                  ~major_collections:1
                  ~compactions:0
                  ~min:980
                  ~max:1_020
                  ~mean:1_020
                  ~median:1_030
                  ~std_dev:5
                  ~iterations:100
                  ~total_time:102_000
                  ());
              benchmark ~index:2 ~name:"serde decode total (1MB)" 2_000;
            ]
            [])
      in
      let _ =
        write_run_with_name
          ~root
          ~profile:"debug"
          ~run_name:"2026-04-21T11-00-00.000Z-second"
          ~suite_name
          ~suite_run:(suite_run
            [
              benchmark_with_stats
                ~index:1
                ~name:"manual decode from parsed tree (1MB)"
                (stats
                  ~minor_collections:8
                  ~major_collections:3
                  ~compactions:2
                  ~min:880
                  ~max:920
                  ~mean:920
                  ~median:930
                  ~std_dev:4
                  ~iterations:100
                  ~total_time:92_000
                  ());
              benchmark ~index:2 ~name:"serde decode total (1MB)" 1_900;
            ]
            [])
      in
      let context =
        Riot_bench.History.create_run_context
          ~workspace_root:root
          ~profile:"debug"
          ~filter:(Some "manual decode from parsed tree")
          ~partial:true
          ~argv:[
            "riot";
            "bench";
            "-p";
            "serde-json";
            "-f";
            "manual decode from parsed tree";
            "--compare";
            "3";
          ]
          ()
      in
      let current =
        suite_run
          [
            benchmark ~index:1 ~name:"manual decode from parsed tree (1MB)" 800;
            benchmark ~index:2 ~name:"serde decode total (1MB)" 1_800;
          ]
          []
      in
      let history =
        Riot_bench.History.compare_suite_run
          context
          ~package_name:(package_name "serde-json")
          ~suite_name
          ~current
          ~limit:3
        |> Result.expect ~msg:"failed to compare suite history"
      in
      match history.benchmarks with
      | [
          {
            name = "manual decode from parsed tree (1MB)";
            history = manual_history;
            baseline = manual_baseline;
            stability = manual_stability;
            current_cv = Some manual_current_cv;
            baseline_cv = Some manual_baseline_cv;
            _;
          };
          {
            name = "serde decode total (1MB)";
            history = serde_history;
            _;
          };
        ] ->
          if not (Int.equal (List.length manual_history) 2) then
            Error "expected manual decode case to compare against two prior runs"
          else if not (Int.equal (List.length serde_history) 2) then
            Error "expected serde decode case to compare against two prior runs"
          else if not (Int64.equal (Time.Duration.to_nanos manual_baseline.mean) 970L) then
            Error "expected manual decode baseline mean to be the median of prior runs"
          else if not (Int.equal manual_baseline.gc.minor_collections 6) then
            Error "expected manual decode baseline gc.minor_collections to be the median of prior runs"
          else if not (Int.equal manual_baseline.gc.major_collections 2) then
            Error "expected manual decode baseline gc.major_collections to be the median of prior runs"
          else if not (Int.equal manual_baseline.gc.compactions 1) then
            Error "expected manual decode baseline gc.compactions to be the median of prior runs"
          else if not
            (
              match manual_stability with
              | Riot_bench.History.Stable -> true
              | Riot_bench.History.Noisy -> false
            ) then
            Error "expected manual decode case to be classified as stable"
          else if Float.compare manual_current_cv 0.01 != Order.LT then
            Error "expected current CV to stay below 1% for the stable case"
          else if Float.compare manual_baseline_cv 0.01 != Order.LT then
            Error "expected baseline CV to stay below 1% for the stable case"
          else
            Ok ()
      | _ -> Error "expected compare suite run to align benchmark history by case name")

let test_compare_suite_run_marks_noisy_cases = fun _ctx ->
  with_tempdir
    "riot_bench_history_noisy"
    (fun root ->
      let suite_name = "large_json_bench" in
      let _ =
        write_run_with_name
          ~root
          ~profile:"debug"
          ~run_name:"2026-04-21T10-00-00.000Z-first"
          ~suite_name
          ~suite_run:(suite_run
            [
              benchmark_with_stats
                ~index:1
                ~name:"manual decode from parsed tree (1MB)"
                (stats
                  ~min:980
                  ~max:1_020
                  ~mean:1_000
                  ~median:1_000
                  ~std_dev:20
                  ~iterations:100
                  ~total_time:100_000
                  ());
            ]
            [])
      in
      let _ =
        write_run_with_name
          ~root
          ~profile:"debug"
          ~run_name:"2026-04-21T11-00-00.000Z-second"
          ~suite_name
          ~suite_run:(suite_run
            [
              benchmark_with_stats
                ~index:1
                ~name:"manual decode from parsed tree (1MB)"
                (stats
                  ~min:970
                  ~max:1_030
                  ~mean:990
                  ~median:995
                  ~std_dev:18
                  ~iterations:100
                  ~total_time:99_000
                  ());
            ]
            [])
      in
      let context =
        Riot_bench.History.create_run_context
          ~workspace_root:root
          ~profile:"debug"
          ~filter:(Some "manual decode from parsed tree")
          ~partial:true
          ~argv:[
            "riot";
            "bench";
            "-p";
            "serde-json";
            "-f";
            "manual decode from parsed tree";
            "--compare";
            "3";
          ]
          ()
      in
      let current =
        suite_run
          [
            benchmark_with_stats
              ~index:1
              ~name:"manual decode from parsed tree (1MB)"
              (stats
                ~min:800
                ~max:1_300
                ~mean:1_000
                ~median:980
                ~std_dev:120
                ~iterations:100
                ~total_time:100_000
                ());
          ]
          []
      in
      let history =
        Riot_bench.History.compare_suite_run
          context
          ~package_name:(package_name "serde-json")
          ~suite_name
          ~current
          ~limit:3
        |> Result.expect ~msg:"failed to compare suite history"
      in
      match history.benchmarks with
      | [
          {
            stability = Riot_bench.History.Noisy;
            current_cv = Some current_cv;
            baseline_cv = Some baseline_cv;
            _;
          };
        ] ->
          if Float.compare current_cv 0.05 != Order.GT then
            Error "expected current CV to exceed the noisy threshold"
          else if Float.compare baseline_cv 0.05 != Order.LT then
            Error "expected baseline CV to remain below the noisy threshold"
          else
            Ok ()
      | _ -> Error "expected noisy history classification for the benchmark")

let tests = [
  Test.case
    "bench history path uses package suite and run id"
    test_suite_run_path_uses_package_suite_and_run_id;
  Test.case
    "bench history writes self-contained suite json"
    test_save_suite_run_writes_self_contained_json;
  Test.case "bench history skips empty suites" test_save_suite_run_skips_empty_suite;
  Test.case
    "bench history loads recent comparable suite runs"
    test_load_recent_suite_runs_filters_and_orders;
  Test.case
    "bench history aligns case history against current suite run"
    test_compare_suite_run_aligns_case_history;
  Test.case
    "bench history marks noisy cases from coefficient of variation"
    test_compare_suite_run_marks_noisy_cases;
]

let main ~args = Test.Cli.main ~name:"riot_bench_history_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
