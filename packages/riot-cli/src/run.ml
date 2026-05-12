open Std
open Std.Result.Syntax
open Riot_model
open Riot_build
open Riot_run
open ArgParser

module Run_runtime = Riot_run

let out = eprintln

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "run"
  |> about "Run a binary"
  |> ArgParser.allow_trailing_args
  |> args
    [
      positional "name"
      |> required false
      |> help
        "Binary name or remote source to run. Use -p/--package to disambiguate local binaries, or the legacy [package:]binary form";
      option "package"
      |> short 'p'
      |> long "package"
      |> help "Run a binary from a specific package";
      flag "list"
      |> long "list"
      |> help "List runnable binaries in the current workspace";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output for --list";
      flag "release"
      |> long "release"
      |> help "Use the release build profile";
      flag "watch"
      |> short 'w'
      |> long "watch"
      |> help "Watch the selected binary dependency cone and restart on changes";
      flag "update"
      |> long "update"
      |> help "Refresh a cached remote source before running";
      trailing "-- [args]..."
      |> help "Arguments to pass to the binary";
      flag "verbose"
      |> short 'v'
      |> long "verbose"
      |> help "Enable verbose output for run"
      |> count;
    ]

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    "release"
  else
    "debug"

let trailing_args = fun matches -> ArgParser.trailing_args matches

let build_scope_for_binary = Run_runtime.build_scope_for_binary

type target =
  | Local of {
      package_name: Riot_model.Package_name.t option;
      binary_name: string;
    }
  | Remote_source of { source_spec: string; binary_name: string }

type implicit_local_target = {
  package_name: Riot_model.Package_name.t;
  binary_name: string;
}

let no_runnable_binaries_message = fun ?package_name () ->
  let hint = "create one with `riot new --bin ./packages/my-binary`" in
  match package_name with
  | Some package_name ->
      "package '"
      ^ Riot_model.Package_name.to_string package_name
      ^ "' has no runnable binaries; "
      ^ hint
  | None -> "no runnable binaries found; pass a binary name or " ^ hint

let parse_package_name = fun package_name ->
  Riot_model.Package_name.from_string package_name
  |> Result.map_err
    ~fn:(fun error ->
      Failure ("invalid package name '"
      ^ package_name
      ^ "': "
      ^ Riot_model.Package_name.error_message error))

let parse_local_target = fun ?package_filter name ->
  match String.split name ~by:":" with
  | [ package_name; binary_name ] ->
      let* package_name = parse_package_name package_name in
      let* () =
        match package_filter with
        | Some expected_package when not
          (Riot_model.Package_name.equal expected_package package_name) ->
            Error (Failure ("conflicting package filters: got --package "
            ^ Riot_model.Package_name.to_string expected_package
            ^ " and binary target "
            ^ name))
        | _ -> Ok ()
      in
      Ok (Local { package_name = Some package_name; binary_name })
  | _ -> Ok (Local { package_name = package_filter; binary_name = name })

let split_remote_binary = fun raw ->
  match String.last_index raw '@' with
  | Some idx when idx = String.length raw - 1 ->
      Error (Failure ("invalid remote target '" ^ raw ^ "': expected binary name after @"))
  | Some idx when idx > 0 && idx < String.length raw - 1 ->
      Ok (
        String.sub raw ~offset:0 ~len:idx,
        Some (String.sub raw ~offset:(idx + 1) ~len:(String.length raw - idx - 1))
      )
  | _ -> Ok (raw, None)

let default_remote_binary_name = fun source_spec ->
  match Riot_deps.Git_dependency.parse_source_locator source_spec with
  | Ok locator -> locator.repo
  | Error _ -> "main"

let parse_target = fun ?package_filter name ->
  if Riot_deps.Git_dependency.looks_like_remote_spec name then
    match package_filter with
    | Some _ -> Error (Failure "--package cannot be used with remote source targets")
    | None -> (
        match split_remote_binary name with
        | Error _ as err -> err
        | Ok (source_spec, binary_name) ->
            Ok (Remote_source {
              source_spec;
              binary_name = Option.unwrap_or
                ~default:(default_remote_binary_name source_spec)
                binary_name;
            })
      )
  else
    parse_local_target ?package_filter name

let implicit_local_targets = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  let package_matches_filter (pkg: Riot_model.Package.t) =
    match package_filter with
    | Some expected_package -> Riot_model.Package_name.equal expected_package pkg.name
    | None -> true
  in
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Run workspace
  |> List.filter ~fn:Package.is_workspace_member
  |> List.filter ~fn:package_matches_filter
  |> List.flat_map
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      Riot_model.Package.binaries_for_scope Riot_model.Package.Normal pkg
      |> List.map
        ~fn:(fun (bin: Riot_model.Package.binary) -> {
          package_name = pkg.name;
          binary_name = bin.name;
        }))

let resolve_implicit_local_target = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  match implicit_local_targets ?package_filter workspace with
  | [ { package_name; binary_name } ] -> Ok { package_name; binary_name }
  | [] -> (
      match package_filter with
      | Some package_name -> Error (no_runnable_binaries_message ~package_name ())
      | None -> Error (no_runnable_binaries_message ())
    )
  | targets ->
      let rendered =
        targets
        |> List.map
          ~fn:(fun { package_name; binary_name } ->
            Riot_model.Package_name.to_string package_name ^ ":" ^ binary_name)
        |> String.concat ", "
      in
      Error ("multiple runnable binaries found; pass a binary name or --package (" ^ rendered ^ ")")

let json_requested_for_child = fun args -> List.any args ~fn:(fun arg -> String.equal arg "--json")

let write_json_event = fun (json: Data.Json.t) -> println (Data.Json.to_string json)

let run_error_to_json = fun (err: Run_runtime.run_error) ->
  let details =
    match err with
    | Run_runtime.BinaryNotFound { binary_name } ->
        [
          ("kind", Data.Json.String "binary_not_found");
          ("binary_name", Data.Json.String binary_name);
        ]
    | Run_runtime.BinaryNotFoundInPackage { package_name; binary_name } ->
        [
          ("kind", Data.Json.String "binary_not_found_in_package");
          ("package_name", Data.Json.String (Riot_model.Package_name.to_string package_name));
          ("binary_name", Data.Json.String binary_name);
        ]
    | Run_runtime.BuildFailed build_error ->
        [
          ("kind", Data.Json.String "build_failed");
          ("message", Data.Json.String (Riot_build.error_message build_error));
        ]
    | Run_runtime.ArtifactNotFound { package_name; binary_name; reason } ->
        [
          ("kind", Data.Json.String "artifact_not_found");
          ("package_name", Data.Json.String (Riot_model.Package_name.to_string package_name));
          ("binary_name", Data.Json.String binary_name);
          ("reason", Data.Json.String reason);
        ]
    | Run_runtime.ProcessExited status ->
        [
          ("kind", Data.Json.String "process_exited");
          ("status", Data.Json.String (Int.to_string status));
        ]
    | Run_runtime.SystemError reason ->
        [ ("kind", Data.Json.String "system_error"); ("reason", Data.Json.String reason); ]
    | Run_runtime.ExternalTargetLoadFailed { target; error } ->
        [
          ("kind", Data.Json.String "external_target_load_failed");
          ("target", Data.Json.String target);
          ("reason", Data.Json.String (Package_error.message error));
        ]
  in
  Data.Json.Object (("type", Data.Json.String "run.error")
  :: ("message", Data.Json.String (Run_runtime.run_error_message err))
  :: details)

let write_run_error = fun ~mode (err: Run_runtime.run_error) ->
  match mode with
  | Ui.Json -> write_json_event (run_error_to_json err)
  | Ui.Line
  | Ui.TUI -> (
      match err with
      | Run_runtime.ProcessExited _ -> ()
      | Run_runtime.BinaryNotFound { binary_name } ->
          out ("error: binary '" ^ binary_name ^ "' not found")
      | err -> out ("error: " ^ Run_runtime.run_error_message err)
    )

let write_workspace_error = fun ~mode message ->
  match mode with
  | Ui.Json ->
      write_json_event
        (Data.Json.Object [
          ("type", Data.Json.String "run.error");
          ("kind", Data.Json.String "workspace_error");
          ("message", Data.Json.String message);
        ])
  | Ui.Line
  | Ui.TUI -> out ("error: " ^ message)

type run_watch_signal =
  | Run_watch_changed of Path.t list
  | Run_watch_child_exited of (unit, Run_runtime.run_error) result

let run_watch_poll_interval = Time.Duration.from_millis 50

let unique_paths = fun paths ->
  paths
  |> List.sort ~compare:Path.compare
  |> List.unique ~compare:Path.compare

let run_watch_file_events_selector = fun msg ->
  match msg with
  | Fs.FileWatcher.FileEvents events -> Select events
  | _ -> Skip

let run_watch_changed_paths = fun session events ->
  unique_paths
    (Watch.changed_paths session events @ Watch.drain_changed_paths session)

let rec wait_run_watch_change = fun session ->
  let events = receive ~selector:run_watch_file_events_selector () in
  match run_watch_changed_paths session events with
  | [] -> wait_run_watch_change session
  | paths -> paths

let wait_run_watch_signal = fun session running ->
  let rec loop () =
    try
      let events =
        receive ~selector:run_watch_file_events_selector ~timeout:run_watch_poll_interval ()
      in
      match run_watch_changed_paths session events with
      | [] -> loop ()
      | paths -> Run_watch_changed paths
    with
    | Receive_timeout -> (
        match Run_runtime.try_wait_running_binary running with
        | Error err -> Run_watch_child_exited (Error err)
        | Ok None -> loop ()
        | Ok (Some result) -> Run_watch_child_exited result
      )
  in
  loop ()

let run_watch_mode = fun output_mode build_ui_mode ->
  match output_mode with
  | Ui.Json -> Ui.Line
  | Ui.Line
  | Ui.TUI -> build_ui_mode

let resolve_local_watch_target = fun ~workspace target ->
  match target with
  | Remote_source _ -> Error (`Cli "riot run --watch only supports local workspace binaries")
  | Local { package_name; binary_name } ->
      Run_runtime.resolve_binary ~workspace ~package_name ~binary_name
      |> Result.map ~fn:(fun package_name -> (package_name, binary_name))
      |> Result.map_err ~fn:(fun err -> `Run err)

let start_watched_binary = fun
  ~workspace ~on_event ~output_mode ~profile ~args package_name binary_name ->
  match Run_runtime.build_binary
    ~on_event
    {
      workspace;
      package_name = Some package_name;
      binary_name;
      profile;
      args;
    } with
  | Error err ->
      write_run_error ~mode:output_mode err;
      Error err
  | Ok built -> (
      match Run_runtime.start_built_binary ~on_event built with
      | Error err ->
          write_run_error ~mode:output_mode err;
          Error err
      | Ok running -> Ok running
    )

let run_watch_local = fun
  ~workspace ~on_event ~output_mode ~build_ui_mode ~profile ~args package_name binary_name ->
  let mode = run_watch_mode output_mode build_ui_mode in
  let* session = Watch.start ~command:"run" ~workspace ~package_filters:[ package_name ] ~mode in
  let rec loop running =
    match running with
    | None ->
        let paths = wait_run_watch_change session in
        Watch.write_change session paths;
        loop (start ())
    | Some running -> (
        match wait_run_watch_signal session running with
        | Run_watch_child_exited result ->
            Result.iter_err result ~fn:(write_run_error ~mode:output_mode);
            loop None
        | Run_watch_changed paths ->
            Watch.write_change session paths;
            (
              match Run_runtime.terminate_running_binary running with
              | Ok () -> ()
              | Error err -> write_run_error ~mode:output_mode err
            );
            loop (start ())
      )
  and start () =
    match start_watched_binary
      ~workspace
      ~on_event
      ~output_mode
      ~profile
      ~args
      package_name
      binary_name with
    | Ok running -> Some running
    | Error _ -> None
  in
  loop (start ())

let binary_source_label = fun
  ~(workspace:Riot_model.Workspace.t) (binary: Run_runtime.runnable_binary) ->
  match Path.strip_prefix binary.source_path ~prefix:workspace.root with
  | Ok relative_path -> Path.to_string relative_path
  | Error _ -> Path.to_string binary.source_path

let write_binary_list = fun ~(workspace:Riot_model.Workspace.t) binaries ->
  binaries
  |> List.for_each
    ~fn:(fun (binary: Run_runtime.runnable_binary) ->
      println
        (Riot_model.Package_name.to_string binary.package_name
        ^ ":"
        ^ binary.binary_name
        ^ " ("
        ^ binary_source_label ~workspace binary
        ^ ")"))

let write_binary_list_json = fun ~(workspace:Riot_model.Workspace.t) binaries ->
  let binary_kind (binary: Run_runtime.runnable_binary) =
    let path = binary_source_label ~workspace binary in
    if List.contains (String.split path ~by:"/") ~value:"examples" then
      "example"
    else
      "binary"
  in
  let binary_json (binary: Run_runtime.runnable_binary) =
    Data.Json.Object [
      ("kind", Data.Json.String (binary_kind binary));
      ("package", Data.Json.String (Riot_model.Package_name.to_string binary.package_name));
      ("binary", Data.Json.String binary.binary_name);
      ("path", Data.Json.String (binary_source_label ~workspace binary));
      (
        "selector",
        Data.Json.String (Riot_model.Package_name.to_string binary.package_name
        ^ ":"
        ^ binary.binary_name)
      );
    ]
  in
  write_json_event
    (Data.Json.Object [
      ("type", Data.Json.String "RunList");
      ("binaries", Data.Json.Array (List.map binaries ~fn:binary_json));
    ])

let run_with_workspace_info = fun ~workspace ~workspace_error matches ->
  let extra = trailing_args matches in
  let _verbose = ArgParser.get_count matches "verbose" in
  let list_mode = ArgParser.get_flag matches "list" in
  let json_mode = ArgParser.get_flag matches "json" in
  let watch = ArgParser.get_flag matches "watch" in
  let* pkg_filter =
    match ArgParser.get_one matches "package" with
    | None -> Ok None
    | Some package_name ->
        parse_package_name package_name
        |> Result.map ~fn:Option.some
  in
  let update = ArgParser.get_flag matches "update" in
  let profile = profile_of_matches matches in
  let output_mode =
    if list_mode && json_mode then
      Ui.Json
    else if json_requested_for_child extra then
      Ui.Json
    else
      Ui.Line
  in
  let build_ui_mode =
    match output_mode with
    | Ui.Json -> Ui.Json
    | Ui.Line
    | Ui.TUI -> Ui.default_human_mode ()
  in
  if watch && list_mode then
    let message = "riot run --watch does not accept --list" in
    write_workspace_error ~mode:output_mode message;
    Error (Failure message)
  else if json_mode && not list_mode then
    let message =
      "riot run --json is only supported with --list; use `riot run -- --json` to forward JSON to the child binary"
    in
    write_workspace_error ~mode:Ui.Json message;
    Error (Failure message)
  else if list_mode then
    match workspace with
    | None ->
        let message = Option.unwrap_or ~default:"Not in a riot workspace" workspace_error in
        write_workspace_error ~mode:output_mode message;
        Error (Failure message)
    | Some workspace ->
        if Option.is_some (ArgParser.get_one matches "name") then
          let message = "riot run --list does not accept a binary name" in
          write_workspace_error ~mode:output_mode message;
          Error (Failure message)
        else if not (List.is_empty extra) then
          let message = "riot run --list does not accept forwarded arguments" in
          write_workspace_error ~mode:output_mode message;
          Error (Failure message)
        else
          let binaries = Run_runtime.list_binaries workspace ?package_filter:pkg_filter () in
          (
            match output_mode with
            | Ui.Json -> write_binary_list_json ~workspace binaries
            | Ui.Line
            | Ui.TUI -> write_binary_list ~workspace binaries
          );
        Ok ()
  else
    let ui = Ui.make ~mode:build_ui_mode ~profile () in
    let on_event event = Ui.send ui event in
    let resolved_target =
      match ArgParser.get_one matches "name" with
      | Some name -> parse_target ?package_filter:pkg_filter name
      | None -> (
          match workspace with
          | Some workspace ->
              resolve_implicit_local_target ?package_filter:pkg_filter workspace
              |> Result.map
                ~fn:(fun { package_name; binary_name } ->
                  Local { package_name = Some package_name; binary_name })
              |> Result.map_err ~fn:(fun err -> Failure err)
          | None ->
              Error (Failure (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
        )
    in
    match resolved_target with
    | Error (Failure message as err) ->
        write_workspace_error ~mode:output_mode message;
        Error err
    | Error _ as err -> err
    | Ok target ->
        let result =
          if watch then
            match workspace with
            | None ->
                Error (`Cli (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
            | Some workspace -> (
                match resolve_local_watch_target ~workspace target with
                | Error _ as err -> err
                | Ok (package_name, binary_name) ->
                    run_watch_local
                      ~workspace
                      ~on_event
                      ~output_mode
                      ~build_ui_mode
                      ~profile
                      ~args:extra
                      package_name
                      binary_name
                    |> Result.map_err ~fn:(fun err -> `Cli (Exception.to_string err))
              )
          else
            match target with
            | Remote_source { source_spec; binary_name } ->
                Run_runtime.run_source
                  ~on_event
                  {
                    source_spec;
                    binary_name;
                    profile;
                    update;
                    args = extra;
                  }
                |> Result.map_err ~fn:(fun err -> `Run err)
            | Local { package_name; binary_name } -> (
                match workspace with
                | Some workspace ->
                    Run_runtime.run
                      ~on_event
                      {
                        workspace;
                        package_name;
                        binary_name;
                        profile;
                        args = extra;
                      }
                    |> Result.map_err ~fn:(fun err -> `Run err)
                | None ->
                    Error (`Cli (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
              )
        in
        match result with
        | Ok () -> Ok ()
        | Error (`Cli message) ->
            write_workspace_error ~mode:output_mode message;
            Error (Failure message)
        | Error (`Run err) ->
            write_run_error ~mode:output_mode err;
            Error (Failure (Run_runtime.run_error_message err))

let run = fun ~workspace matches ->
  run_with_workspace_info
    ~workspace:(Some workspace)
    ~workspace_error:None
    matches
