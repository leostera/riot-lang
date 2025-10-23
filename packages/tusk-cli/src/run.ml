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
         positional "name" |> help "Binary name to run";
         option "binary" |> short 'b' |> long "binary"
         |> help "Specify which binary to run";
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output for run"
         |> count;
       ]

let pick_name matches =
  match get_one matches "name" with
  | Some n -> Some n
  | None -> get_one matches "binary"

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

      (* Ensure server is running and get client *)
      let cwd =
        Env.current_dir ()
        |> Result.expect ~msg:"Failed to get current directory"
      in
      let workspace =
        Workspace_manager.scan cwd
        |> Result.expect ~msg:"Failed to scan workspace"
      in
      let client =
        Tusk_server.Server_manager.ensure_running ~workspace
        |> Result.expect ~msg:"Failed to start or connect to tusk server"
      in

      (* 1) Find executable by name *)
      match Tusk_client.find_executable client name with
      | Ok (Some (pkg, _binary)) -> (
          match Build.build_command (Some pkg) with
          | Ok () -> (
              match
                Tusk_client.find_artifact client ~package:pkg ~kind:"binary"
                  ~name
              with
              | Ok path -> (
                  Tusk_client.close client;
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
              Error (Failure "build failed"))
      | Ok None ->
          Tusk_client.close client;
          println "error: binary '%s' not found" name;
          Error (Failure "binary not found")
      | Error msg ->
          Tusk_client.close client;
          println "error: %s" msg;
          Error (Failure msg))
