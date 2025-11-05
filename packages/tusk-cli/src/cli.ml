open Std

let cli =
  let open ArgParser in
  let open Arg in
  command "tusk" |> version "0.1.0"
  |> about "OCaml build system and package manager"
  |> args
       [
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output"
         |> count;
       ]
  |> subcommands
       [
         Build.command;
         Clean.command;
         Completions.command;
         (* Fmt.command; *)
         (* TODO: Replace with tusk-fmt package *)
         Install.command;
         Mcp_cmd.command;
         New.command;
         Rpc.command;
         Run.command;
         Server_cmd.command;
         Test_cmd.command;
         command "doc" |> about "Generate documentation";
         command "lsp" |> about "Start OCaml LSP server";
         command "version" |> about "Show tusk version";
       ]

let set_verbosity verbose =
  let verbose = if verbose < 0 then 0 else verbose in
  match verbose with
  | 0 -> Log.(set_level Error)
  | 1 -> Log.(set_level Info)
  | 2 -> Log.(set_level Debug)
  | _ -> Log.(set_level Trace)

let main ~args:argv =
  let open ArgParser in
  (* Using Std.Log and Std.Telemetry instead of custom Tusk_log *)
  Std.Log.set_level Info;
  let _ = Std.Telemetry.start () in

  (* Ensure ~/.tusk directories exist *)
  let _ = Tusk_model.Tusk_dirs.ensure_created () in

  match get_matches cli argv with
  | Error err ->
      print_error err;
      Error (Failure "Argument parsing failed")
  | Ok matches -> (
      let verbose = get_count matches "verbose" in
      set_verbosity verbose;

      match get_subcommand matches with
      | Some ("build", build_matches) -> Build.run build_matches
      | Some ("run", run_matches) -> Run.run run_matches
      | Some ("clean", clean_matches) -> Clean.run clean_matches
      | Some ("completions", completions_matches) ->
          Completions.run completions_matches
      | Some ("new", new_matches) -> New.run new_matches
      | Some ("install", install_matches) -> Install.run install_matches
      | Some ("server", server_matches) -> Server_cmd.run server_matches
      | Some ("rpc", rpc_matches) -> Rpc.run rpc_matches
      | Some ("mcp", mcp_matches) -> Mcp_cmd.run mcp_matches
      | Some ("test", test_matches) -> Test_cmd.run test_matches
      | Some ("fmt", fmt_matches) -> Fmt.run fmt_matches
      | Some ("version", _) ->
          println "tusk 0.1.0";
          Ok ()
      | None -> Ok ()
      | Some (cmd, _) ->
          ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
          Error (Failure (format "Unknown command: %s" cmd)))
