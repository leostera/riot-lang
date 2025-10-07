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
         command "build" |> about "Build packages"
         |> args
              [
                option "package" |> short 'p' |> long "package"
                |> help "Build only the specified package";
              ];
         command "run" |> about "Run a binary"
          |> ArgParser.allow_trailing_args
          |> args
               [
                 positional "name" |> help "Binary name to run";
                 option "binary" |> short 'b' |> long "binary"
                 |> help "Specify which binary to run";
                 flag "verbose" |> short 'v' |> long "verbose"
                 |> help "Enable verbose output for run" |> count;
               ];

         command "clean" |> about "Clean build artifacts";
         command "new"
         |> about "Create a new package"
         |> args [ positional "path" |> help "Path for new package" ];
         command "install"
         |> about "Install a binary to ~/.tusk/bin"
         |> args
              [
                option "binary" |> short 'b' |> long "binary"
                |> help "Binary to install";
              ];
         command "server"
         |> about "Start or manage the tusk server"
         |> args
              [
                option "action"
                |> help "Action: start, stop, kill, or status"
                |> possible_values [ "start"; "stop"; "kill"; "status" ];
              ];
         command "rpc"
         |> about "Send RPC command to server"
         |> subcommands
              [
                command "ping" |> about "Test server connectivity";
                command "workspace" |> about "Get workspace information";
                command "graph" |> about "Get build graph";
                command "build"
                |> about "Build all or specific package"
                |> args [ positional "package" |> help "Package to build" ];
                command "package"
                |> about "Get package details"
                |> args [ positional "name" |> help "Package name" ];
                command "format" |> about "Format a file"
                |> args [ positional "file" |> help "File to format" ];
                command "format-check"
                |> about "Check if file needs formatting"
                |> args [ positional "file" |> help "File to check" ];
                command "format-code" |> about "Format code string"
                |> args
                     [
                       positional "code" |> help "Code to format";
                       positional "hint" |> help "Hint for parsing (optional)";
                     ];
                command "restart" |> about "Restart the server";
                command "shutdown" |> about "Shutdown the server";
              ];
         command "mcp" |> about "Start Model Context Protocol server";
         command "doc" |> about "Generate documentation";
         command "fmt" |> about "Format OCaml code"
         |> args
              [
                flag "check" |> long "check"
                |> help "Check if files need formatting without modifying them";
                flag "quiet" |> short 'q' |> long "quiet"
                |> help "Only show failures, suppress successful file messages";
              ];
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
      | Some ("build", build_matches) ->
          let package = get_one build_matches "package" in
          let args = match package with Some p -> [ "-p"; p ] | None -> [] in
          Build.run args
      | Some ("run", run_matches) -> Run.run run_matches
      | Some ("clean", _) -> Clean.run []
      | Some ("new", new_matches) ->
          (* TODO: Extract positional path argument *)
          New.run []
      | Some ("install", install_matches) ->
          let binary = get_one install_matches "binary" in
          let args = match binary with Some b -> [ "-b"; b ] | None -> [] in
          Install.run args
      | Some ("server", server_matches) ->
          let action = get_one server_matches "action" in
          let args = match action with Some a -> [ a ] | None -> [] in
          Server_cmd.run args
      | Some ("rpc", rpc_matches) -> Rpc.run_with_matches rpc_matches
      | Some ("mcp", _) -> Mcp_cmd.run []
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
          (* No subcommand provided - show help *)
          print_help cli;
          Ok ()
      | Some (cmd, _) ->
          ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
          Error (Failure (format "Unknown command: %s" cmd)))
