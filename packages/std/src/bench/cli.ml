open Global
open Collections
open Arg_parser

let parse_format_to_reporter = fun __tmp1 ->
  match __tmp1 with
  | "default" -> Ok (module Reporter.Default : Reporter.Intf.Intf)
  | "json" -> Ok (module Reporter.Reporter_json : Reporter.Intf.Intf)
  | other -> Error ("Unknown format: " ^ other)

let run_benchmarks_cmd =
  let open Arg_parser.Arg in
  command "run-benchmarks"
  |> about "Run benchmarks"
  |> args
    [
      positional "pattern"
      |> required false
      |> help "Run only benchmarks whose names contain this substring";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      option "format"
      |> long "format"
      |> help "Output format: default, json"
      |> default "default"
      |> possible_values [ "default"; "json" ];
      option "iterations"
      |> long "iterations"
      |> help "Override iterations count for all benchmarks";
      option "warmup"
      |> long "warmup"
      |> help "Override warmup count for all benchmarks";
    ]

let get_suite_info name: Reporter.Intf.suite_info = { name }

let benchmark_name = fun __tmp1 ->
  match __tmp1 with
  | Bench_runner.Single case -> case.Bench_case.name
  | Bench_runner.Compare comp -> comp.Bench_comparison.description

let matches_pattern = fun ~pattern bench_item -> String.contains (benchmark_name bench_item) pattern

let list_benchmarks_cmd =
  let open Arg_parser.Arg in
  command "list-benchmarks"
  |> about "List all benchmarks"
  |> args
    [
      positional "pattern"
      |> required false
      |> help "List only benchmarks whose names contain this substring";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      option "iterations"
      |> long "iterations"
      |> help "Override iteration count for all benchmarks";
      option "warmup"
      |> long "warmup"
      |> help "Override warmup count for all benchmarks";
    ]

let bench_item_to_json = fun index item ->
  match item with
  | Bench_runner.Single case ->
      Data.Json.Object [
        ("index", Data.Json.Int index);
        ("name", Data.Json.String case.Bench_case.name);
        ("kind", Data.Json.String "benchmark");
        ("iterations", Data.Json.Int case.config.iterations);
        ("warmup", Data.Json.Int case.config.warmup);
        ("skip", Data.Json.Bool case.skip);
      ]
  | Bench_runner.Compare comp ->
      Data.Json.Object [
        ("index", Data.Json.Int index);
        ("name", Data.Json.String comp.Bench_comparison.description);
        ("kind", Data.Json.String "comparison");
        ("iterations", Data.Json.Int comp.config.iterations);
        ("warmup", Data.Json.Int comp.config.warmup);
        (
          "cases",
          Data.Json.Array (List.map
            comp.cases
            ~fn:(fun (case: Bench_case.t) -> Data.Json.String case.name))
        );
      ]

let list_benchmarks = fun ~json benchmarks ->
  if json then
    let rec to_json_items index items =
      match items with
      | [] -> []
      | item :: rest -> bench_item_to_json index item :: to_json_items (index + 1) rest
    in
    let payload =
      benchmarks
      |> to_json_items 1
      |> fun benchmarks -> Data.Json.Object [ ("benchmarks", Data.Json.Array benchmarks); ]
    in
    print (Data.Json.to_string payload);
    print "\n"
  else
    List.for_each
      benchmarks
      ~fn:(fun item ->
        match item with
        | Bench_runner.Single case -> println case.Bench_case.name
        | Bench_runner.Compare comp -> println (comp.Bench_comparison.description ^ " (comparison)"));
  Ok ()

(** Apply CLI overrides to benchmark items *)
let apply_overrides = fun ~iterations_override ~warmup_override benchmarks ->
  match (iterations_override, warmup_override) with
  | (None, None) -> benchmarks
  | _ ->
      List.map
        benchmarks
        ~fn:(fun item ->
          match item with
          | Bench_runner.Single case ->
              let config = case.Bench_case.config in
              let new_config = {
                Bench_case.iterations = Option.unwrap_or
                  iterations_override
                  ~default:config.iterations;
                warmup = Option.unwrap_or warmup_override ~default:config.warmup;
              }
              in
              Bench_runner.Single { case with config = new_config }
          | Bench_runner.Compare comp ->
              (* Apply to all cases in comparison *)
              let new_cases =
                List.map
                  comp.Bench_comparison.cases
                  ~fn:(fun (case: Bench_case.t) ->
                    let config = case.config in
                    let new_config = {
                      Bench_case.iterations = Option.unwrap_or
                        iterations_override
                        ~default:config.iterations;
                      warmup = Option.unwrap_or warmup_override ~default:config.warmup;
                    }
                    in
                    { case with config = new_config })
              in
              Bench_runner.Compare { comp with cases = new_cases })

let main = fun ~name ~benchmarks ~args ->
  let suite_info = get_suite_info name in
  let cmd =
    command name
    |> about ("Benchmark runner for " ^ name)
    |> subcommands [ list_benchmarks_cmd; run_benchmarks_cmd ]
  in
  match get_matches cmd args with
  | Error err ->
      print_error err;
      Error (Failure (error_message err))
  | Ok matches ->
      match get_subcommand matches with
      | Some ("list-benchmarks", sub_matches) ->
          let pattern = get_one sub_matches "pattern" in
          let iterations_override = get_int sub_matches "iterations" in
          let warmup_override = get_int sub_matches "warmup" in
          let benchmarks_to_list =
            apply_overrides ~iterations_override ~warmup_override benchmarks
          in
          let benchmarks_to_list =
            match pattern with
            | Some pattern -> List.filter benchmarks_to_list ~fn:(matches_pattern ~pattern)
            | None -> benchmarks_to_list
          in
          list_benchmarks ~json:(get_flag sub_matches "json") benchmarks_to_list
      | Some ("run-benchmarks", sub_matches) ->
          let pattern = get_one sub_matches "pattern" in
          let format_str =
            if get_flag sub_matches "json" then
              "json"
            else
              get_one sub_matches "format"
              |> Option.unwrap_or ~default:"default"
          in
          let iterations_override = get_int sub_matches "iterations" in
          let warmup_override = get_int sub_matches "warmup" in
          (* Apply overrides if specified *)
          let benchmarks_to_run =
            apply_overrides ~iterations_override ~warmup_override benchmarks
          in
          let benchmarks_to_run =
            match pattern with
            | Some pattern -> List.filter benchmarks_to_run ~fn:(matches_pattern ~pattern)
            | None -> benchmarks_to_run
          in
          (
            match parse_format_to_reporter format_str with
            | Error msg ->
                println ("Error: " ^ msg);
                Error (Failure msg)
            | Ok reporter ->
                let config = Bench_runner.{ reporter; suite_info } in
                let summary = Bench_runner.run_benchmarks ~config benchmarks_to_run in
                if summary.failed > 0 then
                  System.exit 1;
                Ok ()
          )
      | _ ->
          (* Default: run benchmarks with no overrides *)
          let config = Bench_runner.{ reporter = (module Reporter.Default); suite_info } in
          let summary = Bench_runner.run_benchmarks ~config benchmarks in
          if summary.failed > 0 then
            System.exit 1;
          Ok ()
