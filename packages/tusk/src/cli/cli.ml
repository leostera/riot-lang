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
         Fmt.command;
         Install.command;
         Mcp_cmd.command;
         New.command;
         Rpc.command;
         Run.command;
         Server_cmd.command;
         command "doc" |> about "Generate documentation";
         command "lsp" |> about "Start OCaml LSP server";
         command "version" |> about "Show tusk version";
       ]

let main ~args:argv =
  let open ArgParser in
  let _logger_pid = Core.Tusk_log.init () in

  match get_matches cli argv with
  | Error err ->
      print_error err;
      Error (Failure "Argument parsing failed")
  | Ok matches -> (
      let verbose = get_count matches "verbose" in
      (* TODO: Set log level based on verbose count *)

      match get_subcommand matches with
      | Some ("build", build_matches) -> Build.run build_matches
      | Some ("run", run_matches) -> Run.run run_matches
      | Some ("clean", clean_matches) -> Clean.run clean_matches
      | Some ("new", new_matches) -> New.run new_matches
      | Some ("install", install_matches) -> Install.run install_matches
      | Some ("server", server_matches) -> Server_cmd.run server_matches
      | Some ("rpc", rpc_matches) -> Rpc.run rpc_matches
      | Some ("mcp", mcp_matches) -> Mcp_cmd.run mcp_matches
      | Some ("doc", _) ->
          println "doc command not yet implemented";
          Ok ()
      | Some ("fmt", fmt_matches) -> Fmt.run fmt_matches
      | Some ("lsp", _) ->
          println "lsp command not yet implemented";
          Ok ()
      | Some ("version", _) ->
          println "tusk 0.1.0";
          Ok ()
      | None ->
          print_help cli;
          Ok ()
      | Some (cmd, _) ->
          ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
          Error (Failure (format "Unknown command: %s" cmd)))
