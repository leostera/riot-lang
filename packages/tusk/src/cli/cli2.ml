open Std

let usage_msg =
  {|
   tusk - OCaml build system

   Usage: tusk [COMMAND] [OPTIONS]

   Commands:
     build    Build packages
     clean    Clean build artifacts
     doc      Generate documentation
     fmt      Format OCaml code
     help     Show this help message
     install  Install a package binary to ~/.tusk/bin/
     lsp      Start OCaml LSP server
     mcp      Start Model Context Protocol server
     new      Create a new package
     rpc      Send RPC command to server
     run      Run a binary
     server   Start the tusk server (for debugging)
     version  Show tusk version
|}

let cli =
  ArgParser.command "tusk" ~version:(Some "0.1.0")
    ~about:(Some "OCaml build system and package manager") ()
  |> ArgParser.arg
       Arg.(
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output" |> count)
  |> ArgParser.subcommand
       (ArgParser.command "build" ~about:(Some "Build packages") ()
       |> ArgParser.arg
            Arg.(
              option "package" |> short 'p' |> long "package"
              |> help "Build only the specified package"))
  |> ArgParser.subcommand
       (ArgParser.command "run" ~about:(Some "Run a binary") ()
       |> ArgParser.arg
            Arg.(
              option "binary" |> short 'b' |> long "binary"
              |> help "Specify which binary to run"))
  |> ArgParser.subcommand
       (ArgParser.command "clean" ~about:(Some "Clean build artifacts") ())
  |> ArgParser.subcommand
       (ArgParser.command "new" ~about:(Some "Create a new package") ()
       |> ArgParser.arg Arg.(positional "path" |> help "Path for new package"))
  |> ArgParser.subcommand
       (ArgParser.command "install"
          ~about:(Some "Install a binary to ~/.tusk/bin") ()
       |> ArgParser.arg
            Arg.(
              option "binary" |> short 'b' |> long "binary"
              |> help "Binary to install"))
  |> ArgParser.subcommand
       (ArgParser.command "server"
          ~about:(Some "Start or manage the tusk server") ()
       |> ArgParser.arg
            Arg.(
              option "action" |> help "Action: start, stop, kill, or status"
              |> possible_values [ "start"; "stop"; "kill"; "status" ]))
  |> ArgParser.subcommand
       (ArgParser.command "rpc" ~about:(Some "Send RPC command to server") ())
  |> ArgParser.subcommand
       (ArgParser.command "mcp"
          ~about:(Some "Start Model Context Protocol server") ())
  |> ArgParser.subcommand
       (ArgParser.command "version" ~about:(Some "Show tusk version") ())
  |> ArgParser.subcommand
       (ArgParser.command "help" ~about:(Some "Show help message") ())

let main ~args =
  let argc = List.length args in
  let _logger_pid = Core.Tusk_log.init () in

  if argc < 1 then (
    println "%s" usage_msg;
    Error (Failure "No command specified"))
  else
    match ArgParser.get_matches cli args with
    | Error err ->
        ArgParser.print_error err;
        Error (Failure "Argument parsing failed")
    | Ok matches -> (
        let verbose = ArgParser.get_count matches "verbose" in
        (* TODO: Set log level based on verbose count *)

        match ArgParser.subcommand matches with
        | Some ("build", build_matches) ->
            let package = ArgParser.get_one build_matches "package" in
            let args = match package with Some p -> [ "-p"; p ] | None -> [] in
            Build.run args
        | Some ("run", run_matches) ->
            let binary = ArgParser.get_one run_matches "binary" in
            let args = match binary with Some b -> [ "-b"; b ] | None -> [] in
            (* TODO: Pass remaining args to binary *)
            Ok ()
        | Some ("clean", _) -> Clean.run []
        | Some ("new", new_matches) ->
            (* TODO: Extract positional path argument *)
            New.run []
        | Some ("install", install_matches) ->
            let binary = ArgParser.get_one install_matches "binary" in
            let args = match binary with Some b -> [ "-b"; b ] | None -> [] in
            Install.run args
        | Some ("server", server_matches) ->
            let action = ArgParser.get_one server_matches "action" in
            let args = match action with Some a -> [ a ] | None -> [] in
            Server_cmd.run args
        | Some ("rpc", _) -> Rpc.run []
        | Some ("mcp", _) -> Mcp_cmd.run []
        | Some ("version", _) ->
            println "tusk 0.1.0";
            Ok ()
        | Some ("help", _) | None ->
            println "%s" usage_msg;
            Ok ()
        | Some (cmd, _) ->
            println "Unknown command: %s\n\n%s" cmd usage_msg;
            Error (Failure (format "Unknown command: %s" cmd)))
