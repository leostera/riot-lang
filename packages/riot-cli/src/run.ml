open Std
open Riot_model
open Riot_build
open ArgParser

let command =
  let open ArgParser in
    let open Arg in
      command "run" |> about "Run a binary" |> ArgParser.allow_trailing_args |> args
        [ positional "name" |> help
            "Binary name to run. Use -p/--package to disambiguate, or the \
               legacy [package:]binary form"; option "package"
          |> short 'p'
          |> long "package"
          |> help "Run a binary from a specific package"; trailing "-- [args]..." |> help "Arguments to pass to the binary"; flag
            "verbose"
          |> short 'v'
          |> long "verbose"
          |> help "Enable verbose output for run"
          |> count; ]

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let build_scope_for_binary = Riot_build.build_scope_for_binary

let parse_binary_target = fun ?package_filter name ->
  match String.split_on_char ':' name with
  | [package_name;binary_name] -> (
      match package_filter with
      | Some expected_package when not (String.equal expected_package package_name) -> Error (Failure ("conflicting package filters: got --package "
      ^ expected_package
      ^ " and binary target "
      ^ name))
      | _ -> Ok (Some package_name, binary_name)
    )
  | _ -> Ok (package_filter, name)

let json_requested_for_child = fun args ->
  List.exists (fun arg -> String.equal arg "--json") args

let write_json_event = fun (json: Data.Json.t) ->
  print (Data.Json.to_string json);
  print "\n"

let run_error_to_json = fun (err: Riot_build.run_error) ->
  let details =
    match err with
    | Riot_build.BinaryNotFound { binary_name } ->
        [
          ("kind", Data.Json.String "binary_not_found");
          ("binary_name", Data.Json.String binary_name);
        ]
    | Riot_build.BinaryNotFoundInPackage { package_name; binary_name } ->
        [
          ("kind", Data.Json.String "binary_not_found_in_package");
          ("package_name", Data.Json.String package_name);
          ("binary_name", Data.Json.String binary_name);
        ]
    | Riot_build.BuildFailed build_error ->
        [
          ("kind", Data.Json.String "build_failed");
          ("message", Data.Json.String (Riot_build.build_error_message build_error));
        ]
    | Riot_build.ArtifactNotFound { package_name; binary_name; reason } ->
        [
          ("kind", Data.Json.String "artifact_not_found");
          ("package_name", Data.Json.String package_name);
          ("binary_name", Data.Json.String binary_name);
          ("reason", Data.Json.String reason);
        ]
    | Riot_build.ProcessExited status ->
        [
          ("kind", Data.Json.String "process_exited");
          ("status", Data.Json.String (Int.to_string status));
        ]
    | Riot_build.SystemError reason ->
        [
          ("kind", Data.Json.String "system_error");
          ("reason", Data.Json.String reason);
        ]
    | Riot_build.ClientError client_error ->
        [
          ("kind", Data.Json.String "client_error");
          ("message", Data.Json.String (Riot_build.Client.error_message client_error));
        ]
  in
  Data.Json.Object
    (("type", Data.Json.String "run.error")
    :: ("message", Data.Json.String (Riot_build.run_error_message err))
    :: details)

let write_run_event = fun ~mode (event: Riot_build.run_event) ->
  match mode with
  | Build.Json ->
      Riot_build.run_event_to_json event |> Option.iter write_json_event
  | Build.Human -> (
      match event with
      | Riot_build.Build _ -> ()
      | Riot_build.RunningBinary { package; binary; _ } -> println
        ("    \027[1;32mBuilding\027[0m " ^ package ^ ":" ^ binary)
    )

let write_run_error = fun ~mode (err: Riot_build.run_error) ->
  match mode with
  | Build.Json -> write_json_event (run_error_to_json err)
  | Build.Human -> (
      match err with
      | Riot_build.BinaryNotFound { binary_name } -> println
        ("error: binary '" ^ binary_name ^ "' not found")
      | err -> println ("error: " ^ Riot_build.run_error_message err)
    )

let run = fun ~workspace matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let displayed_packages = Collections.HashSet.create () in
  let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  match ArgParser.get_one matches "name" with
  | None ->
      println "error: missing binary name";
      Error (Failure "missing binary name")
  | Some name -> (
      let extra = trailing_args matches in
      let _verbose = ArgParser.get_count matches "verbose" in
      let pkg_filter = ArgParser.get_one matches "package" in
      match parse_binary_target ?package_filter:pkg_filter name with
      | Error _ as err -> err
      | Ok (package_name, binary_name) ->
          let output_mode =
            if json_requested_for_child extra then
              Build.Json
            else
              Build.Human
          in
          let on_event (event: Riot_build.run_event) =
            match event with
            | Riot_build.Build build_event -> (
                match build_event with
                | Riot_build.Pm kind -> Build.write_pm_event
                  ~mode:output_mode
                  ~seen_registry_updates
                  kind
                | Riot_build.BuildingTarget { target; host } -> Build.write_building_target_event
                  ~mode:output_mode
                  ~target
                  ~host
                | Riot_build.CacheGc event -> Build.write_cache_gc_event
                  ~mode:output_mode
                  event
                | Riot_build.Streaming streaming_event -> Build.write_streaming_event
                  ~mode:output_mode
                  ~displayed_packages
                  ~progress
                  streaming_event
              )
            | _ -> write_run_event ~mode:output_mode event
          in
          match Riot_build.run ~on_event { workspace; package_name; binary_name; args = extra } with
          | Ok () -> Ok ()
          | Error err ->
              write_run_error ~mode:output_mode err;
              Error (Failure (Riot_build.run_error_message err))
    )
