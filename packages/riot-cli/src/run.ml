open Std
open Riot_model
open Riot_build
open ArgParser

module Run_runtime = Riot_build.Commands.Run

let command =
  let open ArgParser in
    let open Arg in command "run"
    |> about "Run a binary"
    |> ArgParser.allow_trailing_args
    |> args
      [
        positional "name" |> required false |> help "Binary name or remote source to run. Use -p/--package to disambiguate local binaries, or the legacy [package:]binary form";
        option "package" |> short 'p' |> long "package" |> help "Run a binary from a specific package";
        flag "list" |> long "list" |> help "List runnable binaries in the current workspace";
        flag "json" |> long "json" |> help "Emit machine-readable JSON output for --list";
        flag "release" |> long "release" |> help "Use the release build profile";
        flag "update" |> long "update" |> help "Refresh a cached remote source before running";
        trailing "-- [args]..." |> help "Arguments to pass to the binary";
        flag "verbose" |> short 'v' |> long "verbose" |> help "Enable verbose output for run" |> count;
      ]

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    "release"
  else
    "debug"

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let build_scope_for_binary = Run_runtime.build_scope_for_binary

type target =
  | Local of { package_name: string option; binary_name: string }
  | Remote_source of { source_spec: string; binary_name: string }

type implicit_local_target = {
  package_name: string;
  binary_name: string;
}

let no_runnable_binaries_message = fun ?package_name () ->
  let hint = "create one with `riot new --bin ./packages/my-binary`" in
  match package_name with
  | Some package_name -> "package '" ^ package_name ^ "' has no runnable binaries; " ^ hint
  | None -> "no runnable binaries found; pass a binary name or " ^ hint

let parse_local_target = fun ?package_filter name ->
  match String.split name ~by:":" with
  | [package_name;binary_name] -> (
      match package_filter with
      | Some expected_package when not (String.equal expected_package package_name) -> Error (Failure ("conflicting package filters: got --package "
      ^ expected_package
      ^ " and binary target "
      ^ name))
      | _ -> Ok (Local { package_name = Some package_name; binary_name })
    )
  | _ -> Ok (Local { package_name = package_filter; binary_name = name })

let split_remote_binary = fun raw ->
  match String.last_index raw '@' with
  | Some idx when idx = String.length raw - 1 -> Error (Failure ("invalid remote target '" ^ raw ^ "': expected binary name after @"))
  | Some idx when idx > 0 && idx < String.length raw - 1 -> Ok (
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
        | Ok (source_spec, binary_name) -> Ok (Remote_source {
          source_spec;
          binary_name = Option.unwrap_or ~default:(default_remote_binary_name source_spec) binary_name
        })
      )
  else
    parse_local_target ?package_filter name

let implicit_local_targets = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  let package_matches_filter (pkg: Riot_model.Package.t) =
    match package_filter with
    | Some expected_package -> String.equal expected_package pkg.name
    | None -> true
  in
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Run workspace
  |> List.filter ~fn:Package.is_workspace_member
  |> List.filter ~fn:package_matches_filter
  |> List.flat_map ~fn:(fun (pkg: Riot_model.Package.t) ->
      Riot_model.Package.binaries_for_scope Riot_model.Package.Normal pkg
      |> List.map ~fn:(fun (bin: Riot_model.Package.binary) -> { package_name = pkg.name; binary_name = bin.name }))

let resolve_implicit_local_target = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  match implicit_local_targets ?package_filter workspace with
  | [ { package_name; binary_name } ] ->
      Ok { package_name; binary_name }
  | [] -> (
      match package_filter with
      | Some package_name -> Error (no_runnable_binaries_message ~package_name ())
      | None -> Error (no_runnable_binaries_message ())
    )
  | targets ->
      let rendered = targets
      |> List.map ~fn:(fun { package_name; binary_name } -> package_name ^ ":" ^ binary_name)
      |> String.concat ", " in
      Error ("multiple runnable binaries found; pass a binary name or --package (" ^ rendered ^ ")")

let json_requested_for_child = fun args ->
  List.any args ~fn:(fun arg -> String.equal arg "--json")

let write_json_event = fun (json: Data.Json.t) ->
  print (Data.Json.to_string json);
  print "\n"

let run_error_to_json = fun (err: Run_runtime.run_error) ->
  let details =
    match err with
    | Run_runtime.BinaryNotFound { binary_name } -> [
      ("kind", Data.Json.String "binary_not_found");
      ("binary_name", Data.Json.String binary_name)
    ]
    | Run_runtime.BinaryNotFoundInPackage { package_name; binary_name } -> [
      ("kind", Data.Json.String "binary_not_found_in_package");
      ("package_name", Data.Json.String package_name);
      ("binary_name", Data.Json.String binary_name);
    ]
    | Run_runtime.BuildFailed build_error -> [
      ("kind", Data.Json.String "build_failed");
      ("message", Data.Json.String (Riot_build.error_message build_error));
    ]
    | Run_runtime.ArtifactNotFound { package_name; binary_name; reason } -> [
      ("kind", Data.Json.String "artifact_not_found");
      ("package_name", Data.Json.String package_name);
      ("binary_name", Data.Json.String binary_name);
      ("reason", Data.Json.String reason);
    ]
    | Run_runtime.ProcessExited status -> [
      ("kind", Data.Json.String "process_exited");
      ("status", Data.Json.String (Int.to_string status))
    ]
    | Run_runtime.SystemError reason -> [
      ("kind", Data.Json.String "system_error");
      ("reason", Data.Json.String reason)
    ]
    | Run_runtime.ExternalTargetLoadFailed { target; reason } -> [
      ("kind", Data.Json.String "external_target_load_failed");
      ("target", Data.Json.String target);
      ("reason", Data.Json.String reason);
    ]
    | Run_runtime.ClientError _ -> [
      ("kind", Data.Json.String "client_error");
      ("message", Data.Json.String (Run_runtime.run_error_message err));
    ]
  in
  Data.Json.Object (("type", Data.Json.String "run.error")
  :: ("message", Data.Json.String (Run_runtime.run_error_message err))
  :: details)

let write_run_event = fun ~mode (event: Run_runtime.run_event) ->
  match mode with
  | Build.Json -> Run_runtime.run_event_to_json event |> Option.for_each ~fn:write_json_event
  | Build.Human -> (
      match event with
      | Run_runtime.Build _ -> ()
      | Run_runtime.RunningBinary { package; binary; _ } -> println
        ("    \027[1;32mBuilding\027[0m " ^ package ^ ":" ^ binary)
    )

let write_run_error = fun ~mode (err: Run_runtime.run_error) ->
  match mode with
  | Build.Json -> write_json_event (run_error_to_json err)
  | Build.Human -> (
      match err with
      | Run_runtime.BinaryNotFound { binary_name } -> println
        ("error: binary '" ^ binary_name ^ "' not found")
      | err -> println ("error: " ^ Run_runtime.run_error_message err)
    )

let write_workspace_error = fun ~mode message ->
  match mode with
  | Build.Json -> write_json_event
    (Data.Json.Object [
      ("type", Data.Json.String "run.error");
      ("kind", Data.Json.String "workspace_error");
      ("message", Data.Json.String message);
    ])
  | Build.Human -> println ("error: " ^ message)

let binary_source_label = fun ~(workspace:Riot_model.Workspace.t) (
  binary: Run_runtime.runnable_binary
) ->
  match Path.strip_prefix binary.source_path ~prefix:workspace.root with
  | Ok relative_path -> Path.to_string relative_path
  | Error _ -> Path.to_string binary.source_path

let write_binary_list = fun ~(workspace:Riot_model.Workspace.t) binaries ->
  binaries
  |> List.for_each ~fn:(fun (binary: Run_runtime.runnable_binary) ->
      println
        (binary.package_name
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
  let binary_json (binary: Run_runtime.runnable_binary) = Data.Json.Object [
    ("kind", Data.Json.String (binary_kind binary));
    ("package", Data.Json.String binary.package_name);
    ("binary", Data.Json.String binary.binary_name);
    ("path", Data.Json.String (binary_source_label ~workspace binary));
    ("selector", Data.Json.String (binary.package_name ^ ":" ^ binary.binary_name));
  ] in
  write_json_event
    (Data.Json.Object [
      ("type", Data.Json.String "RunList");
      ("binaries", Data.Json.Array (List.map binaries ~fn:binary_json));
    ])

let run_with_workspace_info = fun ~workspace ~workspace_error matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let extra = trailing_args matches in
  let _verbose = ArgParser.get_count matches "verbose" in
  let list_mode = ArgParser.get_flag matches "list" in
  let json_mode = ArgParser.get_flag matches "json" in
  let pkg_filter = ArgParser.get_one matches "package" in
  let update = ArgParser.get_flag matches "update" in
  let profile = profile_of_matches matches in
  let output_mode =
    if list_mode && json_mode then
      Build.Json
    else if json_requested_for_child extra then
      Build.Json
    else
      Build.Human
  in
  if json_mode && not list_mode then
    let message = "riot run --json is only supported with --list; use `riot run -- --json` to forward JSON to the child binary" in
    write_workspace_error ~mode:Build.Json message;
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
            | Build.Json -> write_binary_list_json ~workspace binaries
            | Build.Human -> write_binary_list ~workspace binaries
          );
          Ok ()
  else
    let on_event (event: Run_runtime.run_event) =
      match event with
      | Run_runtime.Build build_event -> (
          match build_event with
          | Riot_build.Event.Pm kind -> Build.write_pm_event ~mode:output_mode ~seen_registry_updates kind
          | Riot_build.Event.BuildingTarget { target; host } -> Build.write_building_target_event
            ~mode:output_mode
            ~target
            ~host
          | Riot_build.Event.CacheGc event -> Build.write_cache_gc_event ~mode:output_mode event
          | Riot_build.Event.Phase _ -> ()
        )
      | _ -> write_run_event ~mode:output_mode event
    in
    let resolved_target =
      match ArgParser.get_one matches "name" with
      | Some name -> parse_target ?package_filter:pkg_filter name
      | None -> (
          match workspace with
          | Some workspace -> resolve_implicit_local_target ?package_filter:pkg_filter workspace
          |> Result.map ~fn:(fun { package_name; binary_name } ->
              Local { package_name = Some package_name; binary_name })
          |> Result.map_err ~fn:(fun err -> Failure err)
          | None -> Error (Failure (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
        )
    in
    match resolved_target with
    | Error (Failure message as err) ->
        write_workspace_error ~mode:output_mode message;
        Error err
    | Error _ as err ->
        err
    | Ok target ->
        let result =
          match target with
          | Remote_source { source_spec; binary_name } ->
              Run_runtime.run_source ~on_event
                {
                  source_spec;
                  binary_name;
                  profile;
                  update;
                  args = extra;
                } |> Result.map_err ~fn:(fun err -> `Run err)
          | Local { package_name; binary_name } -> (
              match workspace with
              | Some workspace ->
                  Run_runtime.run ~on_event
                    {
                      workspace;
                      package_name;
                      binary_name;
                      profile;
                      args = extra;
                    } |> Result.map_err ~fn:(fun err -> `Run err)
              | None -> Error (`Cli (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
            )
        in
        match result with
        | Ok () ->
            Ok ()
        | Error (`Cli message) ->
            write_workspace_error ~mode:output_mode message;
            Error (Failure message)
        | Error (`Run err) ->
            write_run_error ~mode:output_mode err;
            Error (Failure (Run_runtime.run_error_message err))

let run = fun ~workspace matches ->
  run_with_workspace_info ~workspace:(Some workspace) ~workspace_error:None matches
