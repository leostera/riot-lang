open Std
module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name |> Result.expect ~msg:("invalid package name: " ^ name)

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let read_json = fun path ->
  let content = Fs.read_to_string path |> Result.expect ~msg:"failed to read written history file" in
  Data.Json.of_string content |> Result.expect ~msg:"failed to parse written history json"

let field = fun name fields ->
  List.find fields
    ~fn:(fun (field_name, _) ->
      String.equal field_name name) |> Option.map ~fn:(fun (_, value) -> value)

let test_suite_run_path_uses_package_suite_and_run_id = fun _ctx ->
  with_tempdir "riot_bench_history_path"
    (fun root ->
      let context = Riot_bench.History.create_run_context
        ~workspace_root:root
        ~profile:"debug"
        ~filter:(Some "ioslice")
        ~partial:true
        ~argv:[ "riot"; "bench"; "-p"; "std"; "-f"; "ioslice" ]
        () in
      let path = Riot_bench.History.suite_run_path
        context
        ~package_name:(package_name "std")
        ~suite_name:"std_io_ioslice_bench" in
      let rendered = Path.to_string path in
      let run_id = Riot_bench.History.run_id context in
      if not (String.contains rendered ".riot/bench/std/std_io_ioslice_bench/runs/") then
        Error ("unexpected bench history path: " ^ rendered)
      else if not (String.ends_with ~suffix:(run_id ^ ".json") rendered) then
        Error ("expected bench history path to end with run id, got: " ^ rendered)
      else
        Ok ())

let test_save_suite_run_writes_self_contained_json = fun _ctx ->
  with_tempdir "riot_bench_history_write"
    (fun root ->
      let context = Riot_bench.History.create_run_context
        ~workspace_root:root
        ~profile:"release"
        ~filter:(Some "parser")
        ~partial:true
        ~argv:[ "riot"; "bench"; "-p"; "http"; "-f"; "parser"; "--release" ]
        () in
      let suite_run: Riot_bench.History.suite_run = {
        status = 0;
        started_at_us = Some 12;
        completed_at_us = Some 34;
        duration_us = Some 22;
        summary = { total = 1; completed = 1; skipped = 0; failed = 0 };
        benchmarks =
          [ {
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
                };
            } ];
        comparisons = [];
      }
      in
      let path = Riot_bench.History.save_suite_run
        context
        ~package_name:(package_name "http")
        ~suite_name:"http1_parser_bench"
        ~suite_run
      |> Result.expect ~msg:"failed to save suite history"
      |> Option.expect ~msg:"expected non-empty suite history to be saved" in
      let json = read_json path in
      match json with
      | Data.Json.Object fields -> (
          match field "schema_version" fields, field "run_id" fields, field "suite" fields, field
            "selection"
            fields, field "suite_run" fields with
          | Some (Data.Json.Int 1), Some (Data.Json.String _), Some (Data.Json.Object suite_fields), Some (Data.Json.Object selection_fields), Some (Data.Json.Object suite_run_fields) ->
              let suite_name_ok = field "name" suite_fields
              = Some (Data.Json.String "http1_parser_bench") in
              let package_ok = field "package" suite_fields = Some (Data.Json.String "http") in
              let profile_ok = field "profile" suite_fields = Some (Data.Json.String "release") in
              let partial_ok = field "partial" selection_fields = Some (Data.Json.Bool true) in
              let filter_ok = field "filter" selection_fields = Some (Data.Json.String "parser") in
              let benchmarks_ok =
                match field "benchmarks" suite_run_fields with
                | Some (Data.Json.Array [ Data.Json.Object benchmark_fields ]) -> field "name" benchmark_fields
                = Some (Data.Json.String "small request")
                | _ -> false
              in
              if
                suite_name_ok && package_ok && profile_ok && partial_ok && filter_ok && benchmarks_ok
              then
                Ok ()
              else
                Error "expected saved bench history json to contain suite metadata and benchmark results"
          | _ -> Error "expected saved bench history json to expose the top-level schema, suite, selection, and suite_run fields"
        )
      | _ -> Error "expected saved bench history file to be a json object")

let test_save_suite_run_skips_empty_suite = fun _ctx ->
  with_tempdir "riot_bench_history_empty"
    (fun root ->
      let context = Riot_bench.History.create_run_context
        ~workspace_root:root
        ~profile:"debug"
        ~filter:(Some "ioslice")
        ~partial:true
        ~argv:[ "riot"; "bench"; "-p"; "std"; "-f"; "ioslice" ]
        () in
      let suite_run: Riot_bench.History.suite_run = {
        status = 0;
        started_at_us = Some 10;
        completed_at_us = Some 20;
        duration_us = Some 10;
        summary = { total = 0; completed = 0; skipped = 0; failed = 0 };
        benchmarks = [];
        comparisons = [];
      } in
      let expected_path = Riot_bench.History.suite_run_path
        context
        ~package_name:(package_name "std")
        ~suite_name:"std_io_ioslice_bench" in
      match
        Riot_bench.History.save_suite_run
          context
          ~package_name:(package_name "std")
          ~suite_name:"std_io_ioslice_bench"
          ~suite_run
      with
      | Error error ->
          Error ("expected empty suite history save to be skipped, got error: " ^ error)
      | Ok (Some path) ->
          Error ("expected empty suite history save to skip writing, got: " ^ Path.to_string path)
      | Ok None -> (
          match Fs.exists expected_path with
          | Error err -> Error ("expected fs exists check to succeed: " ^ IO.error_message err)
          | Ok true -> Error "expected empty suite history save to leave no file behind"
          | Ok false -> Ok ()
        ))

let tests = [
  Test.case "bench history path uses package suite and run id" test_suite_run_path_uses_package_suite_and_run_id;
  Test.case "bench history writes self-contained suite json" test_save_suite_run_writes_self_contained_json;
  Test.case "bench history skips empty suites" test_save_suite_run_skips_empty_suite;
]

let () =
  Runtime.run
    ~main:(fun ~args -> Test.Cli.main ~name:"riot_bench_history_tests" ~tests ~args ())
    ~args:Env.args
    ()
