open Std
open Std.Collections
open Std.Sync.Cell
open Tusk_model
open Tusk_model
open ArgParser

let command =
  let open ArgParser in
  let open Arg in
  command "bench" |> about "Run benchmarks with optional pattern matching"
  |> ArgParser.allow_trailing_args
  |> args
       [
          positional "pattern" |> required false
          |> help
               "Benchmark pattern: 'prefix' runs all benchmarks starting with prefix, \
                'pkg:prefix' runs package-scoped benchmarks, 'pkg:...' runs all benchmarks \
                in package. Examples: 'vector_', 'std:vector_', 'poneglyph:...'. Omit to \
                run all benchmarks.";
         option "package" |> short 'p' |> long "package"
         |> help "Run benchmarks from specific package (deprecated, use pattern)";
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output for benchmarks"
         |> count;
       ]

let trailing_args matches =
  let args = ArgParser.trailing_args matches in
  match args with "--" :: rest -> rest | _ -> args

let run matches =
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in

  (* Parse pattern: [pkg][:bench_prefix] *)
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in

  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let client =
    Tusk_server.Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to start or connect to tusk server"
  in

  (* Parse pattern: [pkg]:bench_prefix or pkg:bench_prefix or pkg:... *)
  let (package_filter, bench_prefix) =
    match (pattern, legacy_package) with
    | (Some p, _) -> (
        match String.split_on_char ':' p with
        | [ pkg; "..." ] -> (Some pkg, None) (* pkg:... means all benchmarks in package *)
        | [ pkg; prefix ] -> (Some pkg, Some prefix)
        | [ single ] -> (None, Some single) (* Treat as bench prefix across all packages *)
        | _ -> (None, None))
    | (None, Some pkg) -> (Some pkg, None)
    | (None, None) -> (None, None)
  in

  let packages =
    match package_filter with
    | Some pkg_name ->
        List.filter
          (fun (pkg : Package.t) -> 
            String.equal pkg.name pkg_name && Package.is_workspace_member pkg)
          workspace.packages
    | None -> 
        (* Only benchmark workspace members, not external dependencies *)
        List.filter Package.is_workspace_member workspace.packages
  in

  let bench_binaries =
    List.concat_map
      (fun (pkg : Package.t) ->
        List.filter_map
          (fun (bin : Package.binary) ->
            let is_bench =
              String.ends_with ~suffix:"_bench" bin.name
            in
            let matches_prefix =
              match bench_prefix with
              | None -> true
              | Some prefix -> String.starts_with ~prefix bin.name
            in
            if is_bench && matches_prefix then Some (pkg.name, bin.name)
            else None)
          pkg.binaries)
      packages
  in

  if List.length bench_binaries = 0 then (
    Tusk_client.close client;
    println "No benchmark binaries found";
    (match (package_filter, bench_prefix) with
    | (Some pkg, Some prefix) ->
        println ("Hint: No benchmarks matching '" ^ pkg ^ ":*" ^ prefix ^ "*' found")
    | (Some pkg, None) ->
        println
          ("Hint: Make sure package '" ^ pkg ^ "' has binaries ending in '_bench'")
    | (None, Some prefix) -> println ("Hint: No benchmarks matching '*" ^ prefix ^ "*' found")
    | (None, None) -> println "Hint: Benchmark binaries should end in '_bench'");
    Ok ())
  else
    (* Group benchmark binaries by package *)
    let benches_by_package =
      List.fold_left
        (fun acc (pkg, bench_name) ->
          let existing =
            HashMap.get acc pkg |> Option.unwrap_or ~default:[]
          in
          let _ = HashMap.insert acc pkg (bench_name :: existing) in
          acc)
        (HashMap.create ()) bench_binaries
    in

    let total = List.length bench_binaries in
    let failed = ref 0 in
    let passed = ref 0 in

    (* Build each package once, then run all its benchmarks *)
    HashMap.iter
      (fun pkg bench_names ->
        println "";
        println ("Building package '" ^ pkg ^ "'...");
        match Build.build_command (Some pkg) with
        | Ok () ->
            List.iter
              (fun bench_name ->
                match
                  Tusk_client.find_artifact client ~package:pkg ~kind:"binary"
                    ~name:bench_name
                with
                | Ok path -> (
                    let bench_args =
                      match extra_args with
                      | [] -> [ "run-benchmarks" ]
                      | _ -> "run-benchmarks" :: extra_args
                    in
                    let cmd = Command.make path ~args:bench_args in
                    println "";
                    println ("Running " ^ pkg ^ "/" ^ bench_name ^ "...");
                    println "";
                    match Command.status cmd with
                    | Ok 0 -> incr passed
                    | Ok _code -> incr failed
                    | Error (Command.SystemError msg) ->
                        println ("error: " ^ msg);
                        incr failed)
                | Error msg ->
                    println ("error: " ^ msg);
                    incr failed)
              bench_names
        | Error _ ->
            println ("error: build failed for package '" ^ pkg ^ "'");
            (* Mark all benchmarks in this package as failed *)
            failed := !failed + List.length bench_names)
      benches_by_package;

    Tusk_client.close client;

    println "";
    println "Benchmark Summary:";
    println ("  Total benchmark suites: " ^ Int.to_string total);
    println ("  Passed: " ^ Int.to_string !passed);
    println ("  Failed: " ^ Int.to_string !failed);

    if !failed > 0 then
      Error (Failure (Int.to_string !failed ^ " benchmark suite(s) failed"))
    else Ok ()
