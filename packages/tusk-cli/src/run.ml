open Std
open Tusk_model
open Tusk_model
open ArgParser

let command =
  let open ArgParser in
  let open Arg in
  command "run" |> about "Run a binary" |> ArgParser.allow_trailing_args
  |> args
       [
         positional "name" |> help "Binary name to run (format: [package:]binary)";
         trailing "-- [args]..." |> help "Arguments to pass to the binary";
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output for run"
         |> count;
       ]

let pick_name matches = get_one matches "name"

let trailing_args matches =
  let args = ArgParser.trailing_args matches in
  match args with "--" :: rest -> rest | _ -> args

let run matches =
  match pick_name matches with
  | None ->
      println "error: missing binary name";
      Error (Failure "missing binary name")
  | Some name -> (
      let extra = trailing_args matches in
      let verbose = ArgParser.get_count matches "verbose" in
      let _ = verbose in

      (* Parse pkg:bin format if present *)
      let (pkg_filter, bin_name) =
        match String.split_on_char ':' name with
        | [ pkg; bin ] -> (Some pkg, bin)
        | _ -> (None, name)
      in

      (* Ensure server is running and get client *)
      let cwd =
        Env.current_dir ()
        |> Result.expect ~msg:"Failed to get current directory"
      in
      let (workspace, _load_errors) =
        Workspace_manager.scan cwd
        |> Result.expect ~msg:"Failed to scan workspace"
      in
      let client =
        Tusk_server.Server_manager.ensure_running ~workspace
        |> Result.expect ~msg:"Failed to start or connect to tusk server"
      in

      (* 1) Find executable by name (and optionally filter by package) *)
      match Tusk_client.find_executable client bin_name with
      | Ok (Some (pkg, _binary)) -> (
          (* If pkg_filter specified, verify it matches *)
          match pkg_filter with
          | Some expected_pkg when expected_pkg <> pkg ->
              Tusk_client.close client;
              println "error: binary '%s' not found in package '%s'" bin_name
                expected_pkg;
              Error (Failure "binary not found in specified package")
          | _ -> (
              match Build.build_command (Some pkg) with
              | Ok () -> (
                  match
                    Tusk_client.find_artifact client ~package:pkg ~kind:"binary"
                      ~name:bin_name
                  with
                  | Ok path -> (
                      Tusk_client.close client;
                      println "     \027[1;32mRunning\027[0m %s:%s" pkg bin_name;
                      let cmd = Command.make path ~args:extra in
                      match Command.status cmd with
                      | Ok 0 -> Ok ()
                      | Ok code ->
                          println "error: process exited with %d" code;
                          Error (Failure (format "process exited with %d" code))
                      | Error (Command.SystemError msg) ->
                          println "error: %s" msg;
                          Error (Failure msg))
                  | Error msg ->
                      Tusk_client.close client;
                      println "error: %s" msg;
                      Error (Failure msg))
              | Error _ ->
                  println "error: build failed for package '%s'" pkg;
                  Tusk_client.close client;
                  Error (Failure "build failed")))
      | Ok None ->
          Tusk_client.close client;
          println "error: binary '%s' not found" name;
          Error (Failure "binary not found")
      | Error msg ->
          Tusk_client.close client;
          println "error: %s" msg;
          Error (Failure msg))
