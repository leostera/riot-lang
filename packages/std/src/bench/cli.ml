open Global
open Collections
open Arg_parser

let list_benchmarks = fun benchmarks ->
  List.iter
    (fun item ->
      match item with
      | Bench_runner.Single case -> println case.Bench_case.name
      | Bench_runner.Compare comp -> println (comp.Bench_comparison.description ^ " (comparison)"))
    benchmarks;
  Ok ()

let run_benchmarks_cmd =
  let open Arg in command "run-benchmarks"
  |> about "Run benchmarks"
  |> args
  [
    positional "pattern" |> required false |> help "Run only benchmarks whose names contain this substring";
    option "format" |> long "format" |> help "Output format (currently only 'default')" |> default "default";
    option "iterations" |> long "iterations" |> help "Override iterations count for all benchmarks";
    option "warmup" |> long "warmup" |> help "Override warmup count for all benchmarks";

  ]

let list_benchmarks_cmd = command "list-benchmarks" |> about "List all benchmarks"

let get_suite_info name : Reporter.Intf.suite_info = {name}

let benchmark_name =
  function
  | Bench_runner.Single case -> case.Bench_case.name
  | Bench_runner.Compare comp -> comp.Bench_comparison.description

let matches_pattern = fun ~pattern bench_item ->
  String.contains (benchmark_name bench_item) pattern

(** Apply CLI overrides to benchmark items *)
let apply_overrides = fun ~iterations_override ~warmup_override benchmarks ->
  match (iterations_override, warmup_override) with
  | (None, None) -> benchmarks
  | _ ->
      List.map
        (fun item ->
          match item with
          | Bench_runner.Single case ->
              let config = case.Bench_case.config in
              let new_config = {
                Bench_case.iterations = Option.unwrap_or ~default:config.iterations iterations_override;
                warmup = Option.unwrap_or ~default:config.warmup warmup_override;

              } in
              Bench_runner.Single {case with config = new_config}
          | Bench_runner.Compare comp ->
              (* Apply to all cases in comparison *)
              let new_cases =
                List.map
                  (fun (case: Bench_case.t) ->
                    let config = case.config in
                    let new_config = {
                      Bench_case.iterations = Option.unwrap_or ~default:config.iterations iterations_override;
                      warmup = Option.unwrap_or ~default:config.warmup warmup_override;

                    } in
                    {case with config = new_config})
                  comp.Bench_comparison.cases
              in
              Bench_runner.Compare {comp with cases = new_cases})
        benchmarks

let main = fun ~name ~benchmarks ~args ->
  let suite_info = get_suite_info name in
  let cmd = command name
  |> about ("Benchmark runner for " ^ name)
  |> subcommands [ list_benchmarks_cmd; run_benchmarks_cmd ] in
  match get_matches cmd args with
  | Error err ->
      print_error err;
      Error (Failure (error_message err))
  | Ok matches -> (
      match get_subcommand matches with
      | Some ("list-benchmarks", _) ->
          list_benchmarks benchmarks
      | Some ("run-benchmarks", sub_matches) ->
          let pattern = get_one sub_matches "pattern" in
          let _format = get_one sub_matches "format" |> Option.unwrap_or ~default:"default" in
          let iterations_override = get_int sub_matches "iterations" in
          let warmup_override = get_int sub_matches "warmup" in
          (* Apply overrides if specified *)
          let benchmarks_to_run = apply_overrides ~iterations_override ~warmup_override benchmarks in
          let benchmarks_to_run =
            match pattern with
            | Some pattern -> List.filter (matches_pattern ~pattern) benchmarks_to_run
            | None -> benchmarks_to_run
          in
          let config = Bench_runner.{reporter = (module Reporter.Default); suite_info} in
          let summary = Bench_runner.run_benchmarks ~config benchmarks_to_run in
          if summary.failed > 0 then
            exit 1;
          Ok ()
      | _ ->
          (* Default: run benchmarks with no overrides *)
          let config = Bench_runner.{reporter = (module Reporter.Default); suite_info} in
          let summary = Bench_runner.run_benchmarks ~config benchmarks in
          if summary.failed > 0 then
            exit 1;
          Ok ()
    )
