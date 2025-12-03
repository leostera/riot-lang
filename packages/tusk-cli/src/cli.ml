open Std

(** Build the CLI with dynamically discovered package commands *)
let build_cli workspace_opt =
  let open ArgParser in
  let open Arg in
  
  (* Static built-in commands *)
  let builtin_commands = [
    Build.command;
    Clean.command;
    Completions.command;
    (* Fmt.command; *)
    (* TODO: Replace with tusk-fmt package *)
    Tusk_init.command;
    Install.command;
    Mcp_cmd.command;
    New.command;
    Rpc.command;
    Run.command;
    Server_cmd.command;
    Test_cmd.command;
    Bench_cmd.command;
    Toolchain_cmd.command;
    command "doc" |> about "Generate documentation";
    command "lsp" |> about "Start OCaml LSP server";
    command "version" |> about "Show tusk version";
  ] in
  
  (* Add package commands if we have a workspace *)
  let package_commands = match workspace_opt with
    | None -> []
    | Some workspace ->
        let commands = Tusk_model.Workspace.discover_commands workspace in
        List.map (fun (cmd : Tusk_model.Package_command.t) ->
          (* Use package:command format to avoid conflicts *)
          let namespaced_name = cmd.package_name ^ ":" ^ cmd.name in
          command namespaced_name 
          |> about (cmd.description ^ " (from " ^ cmd.package_name ^ ")")
        ) commands
  in
  
  command "tusk" |> version "0.1.0"
  |> about "OCaml build system and package manager"
  |> args
       [
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output"
         |> count;
       ]
  |> subcommands (builtin_commands @ package_commands)

let set_verbosity verbose =
  let verbose = if verbose < 0 then 0 else verbose in
  match verbose with
  | 0 -> Log.(set_level Error)
  | 1 -> Log.(set_level Info)
  | 2 -> Log.(set_level Debug)
  | _ -> Log.(set_level Trace)

(** Get workspace or return None *)
let get_workspace () =
  match Env.current_dir () with
  | Error _ -> None
  | Ok cwd -> (
      match Tusk_model.Workspace_manager.scan cwd with
      | Error _ -> None
      | Ok (workspace, _load_errors) -> Some workspace)

(** Try to execute a package command if it exists *)
let try_command cmd_name remaining_args =
  match get_workspace () with
  | None -> None
  | Some workspace -> (
      (* Parse package:command format *)
      match String.split_on_char ':' cmd_name with
      | [package_name; command_name] -> (
          (* Find the command in the specified package *)
          let commands = Tusk_model.Workspace.discover_commands workspace in
          match List.find_opt (fun (cmd : Tusk_model.Package_command.t) ->
            cmd.package_name = package_name && cmd.name = command_name
          ) commands with
           | None -> None
           | Some cmd ->
              Log.info ("Found command: " ^ cmd.package_name ^ ":" ^ cmd.name);
              Log.info ("Command binary path: " ^ Path.to_string cmd.command_binary);
              (* Build the package first to ensure command is up to date *)
              Log.info ("Building package: " ^ cmd.package_name);
              (match Build.build_command (Some cmd.package_name) None Tusk_server.Server_config.default with
              | Error err ->
                  Log.error ("Failed to build package: " ^ Exception.to_string err);
                  Some (Error err)
              | Ok () ->
                  (* Execute the command binary *)
                  match Command_executor.execute ~command_binary:cmd.command_binary ~args:remaining_args with
                  | Ok () -> Some (Ok ())
                  | Error err ->
                      Log.error ("Command execution failed: " ^ Exception.to_string err);
                      Some (Error err)))
      | _ -> None)

let ensure_toolchain workspace =
  (* Check toolchain before starting server to provide better error messages *)
  let toolchain_config = Tusk_model.Toolchain_config.from_workspace workspace in
  match Tusk_toolchain.init ~config:toolchain_config with
  | Ok _ -> Ok ()
  | Error msg ->
      println "\n❌ ERROR: Toolchain initialization failed!\n";
      println msg;
      println "";
      Error (Failure "Toolchain not available")

let main ~args =
  (* Load config BEFORE starting logger - handlers need config *)
  Std.Config.load_string {|
[[log.handler]]
type = "stdout"
format = "full"
|};

  Std.Log.set_level Info;
  
  (* Now start logger and telemetry *)
  let _ = Std.Log.start_link () in
  let _ = Std.Telemetry.start () in
  
  (* Ensure ~/.tusk directories exist *)
  Tusk_model.Tusk_dirs.ensure_created () |> Result.expect ~msg:"Could not create tusk dirs";

  (* Try to load workspace for command discovery (silently fail if not in workspace) *)
  let workspace_opt = get_workspace () in
  
  (* Check if first arg is a package command (format: package:command) before ArgParser *)
  match args with
  | _ :: cmd :: rest when String.contains cmd ":" -> (
      (* This looks like a package command, try to execute it directly *)
      match try_command cmd rest with
      | Some result -> result
      | None ->
          (* Not a valid package command, fall through to normal parsing *)
          let cli = build_cli workspace_opt in
          match ArgParser.get_matches cli args with
          | Error err ->
              ArgParser.print_error err;
              Error (Failure "Argument parsing failed")
          | Ok _ ->
              ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
              Error (Failure ("Unknown command: " ^ cmd))
    )
  | _ ->
      (* Normal command parsing *)
      let cli = build_cli workspace_opt in
      match ArgParser.get_matches cli args with
      | Error err ->
          ArgParser.print_error err;
          Error (Failure "Argument parsing failed")
      | Ok matches -> (
      let verbose = ArgParser.get_count matches "verbose" in
      set_verbosity verbose;

      match ArgParser.get_subcommand matches with
      | Some ("build", build_matches) -> 
          (match workspace_opt with
          | Some workspace -> 
              (match ensure_toolchain workspace with
              | Ok () -> Build.run build_matches
              | Error _ as e -> e)
          | None -> Build.run build_matches)
      | Some ("run", run_matches) -> 
          (match workspace_opt with
          | Some workspace -> 
              (match ensure_toolchain workspace with
              | Ok () -> Run.run run_matches
              | Error _ as e -> e)
          | None -> Run.run run_matches)
      | Some ("test", test_matches) -> 
          (match workspace_opt with
          | Some workspace -> 
              (match ensure_toolchain workspace with
              | Ok () -> Test_cmd.run test_matches
              | Error _ as e -> e)
          | None -> Test_cmd.run test_matches)
      | Some ("bench", bench_matches) -> 
          (match workspace_opt with
          | Some workspace -> 
              (match ensure_toolchain workspace with
              | Ok () -> Bench_cmd.run bench_matches
              | Error _ as e -> e)
          | None -> Bench_cmd.run bench_matches)
      | Some ("fmt", fmt_matches) -> 
          (match workspace_opt with
          | Some workspace -> 
              (match ensure_toolchain workspace with
              | Ok () -> Fmt.run fmt_matches
              | Error _ as e -> e)
          | None -> Fmt.run fmt_matches)
      | Some ("clean", clean_matches) -> Clean.run clean_matches
      | Some ("completions", completions_matches) ->
          Completions.run completions_matches
      | Some ("init", init_matches) -> Tusk_init.run init_matches
      | Some ("new", new_matches) -> New.run new_matches
      | Some ("install", install_matches) -> Install.run install_matches
      | Some ("server", server_matches) -> 
          (match workspace_opt with
          | Some workspace -> 
              (match ensure_toolchain workspace with
              | Ok () -> Server_cmd.run server_matches
              | Error _ as e -> e)
          | None -> Server_cmd.run server_matches)
      | Some ("rpc", rpc_matches) -> Rpc.run rpc_matches
      | Some ("mcp", mcp_matches) -> Mcp_cmd.run mcp_matches
      | Some ("toolchain", toolchain_matches) -> Toolchain_cmd.run toolchain_matches
      | Some ("version", _) ->
          println "tusk 0.1.0";
          Ok ()
      | None -> Ok ()
      | Some (cmd, _matches) -> (
          (* Check if this is a package command *)
          (* Extract remaining args after the command name *)
          let remaining_args = 
            match List.tl args with
            | cmd_arg :: rest when cmd_arg = cmd -> rest
            | _ -> []
          in
          match try_command cmd remaining_args with
          | Some result -> result
          | None ->
              ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
              Error (Failure ("Unknown command: " ^ cmd))))
