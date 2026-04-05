open Std

type build_scope =
  | Runtime
  | Dev

type target_request =
  | Host
  | All
  | Pattern of string

type build_request = {
  workspace: Riot_model.Workspace.t;
  packages: string list;
  targets: target_request;
  scope: build_scope;
  profile: string;
}

type build_event = Event.t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Streaming of Client.streaming_event

type build_error =
  | NoTargetsMatched of { pattern: string; available_targets: string list }
  | ToolchainInstallFailed of { target: string; error: string }
  | ToolchainInitializationFailed of { target: string; error: string }
  | ClientError of Client.error

let no_event: build_event -> unit = fun _ -> ()

let error_message = function
  | NoTargetsMatched { pattern; available_targets } -> "No targets match pattern '"
  ^ pattern
  ^ "'. Available targets: "
  ^ String.concat ", " available_targets
  | ToolchainInstallFailed { target; error } -> "Failed to install toolchain for "
  ^ target
  ^ ": "
  ^ error
  | ToolchainInitializationFailed { target; error } -> "Failed to initialize toolchain for "
  ^ target
  ^ ": "
  ^ error
  | ClientError err -> Client.error_message err

let get_configured_targets = fun (workspace: Riot_model.Workspace.t) ->
  let config = Riot_model.Toolchain_config.from_workspace workspace in
  match config.targets with
  | [] -> [ Riot_toolchain.get_host_triple () ]
  | targets -> targets

let resolve_target_pattern = fun workspace pattern ->
  let configured = get_configured_targets workspace in
  let host = Riot_toolchain.get_host_triple () in
  match String.lowercase_ascii pattern with
  | "host"
  | "native" ->
      Ok [ host ]
  | "all" ->
      Ok configured
  | exact when List.mem exact configured ->
      Ok [ exact ]
  | pattern ->
      let matches =
        List.filter
          (fun target ->
            String.contains target pattern)
          configured
      in
      if List.length matches = 0 then
        Error (NoTargetsMatched { pattern; available_targets = configured })
      else
        Ok matches

let resolve_targets = fun (request: build_request) ->
  match request.targets with
  | Host -> Ok [ Riot_toolchain.get_host_triple () ]
  | All -> Ok (get_configured_targets request.workspace)
  | Pattern pattern -> resolve_target_pattern request.workspace pattern

let ensure_toolchains_for_targets = fun workspace targets ->
  let config = Riot_model.Toolchain_config.from_workspace workspace in
  let missing =
    List.filter
      (fun target ->
        match Riot_toolchain.check_toolchain_status ~version:config.version ~target with
        | Riot_toolchain.NotInstalled _
        | Riot_toolchain.Incomplete _ -> true
        | Riot_toolchain.Installed _ -> false)
      targets
  in
  let host = Riot_toolchain.get_host_triple () in
  let rec loop = function
    | [] -> Ok ()
    | target :: rest -> (
        match Riot_toolchain.download_and_install_toolchain config.version ~host ~target with
        | Ok () -> loop rest
        | Error error -> Error (ToolchainInstallFailed { target; error })
      )
  in
  loop missing

let validate_target_toolchains = fun workspace targets ->
  let config = Riot_model.Toolchain_config.from_workspace workspace in
  let rec loop = function
    | [] -> Ok ()
    | target :: rest -> (
        match Riot_toolchain.init_for_target ~config ~target with
        | Ok _ -> loop rest
        | Error error -> Error (ToolchainInitializationFailed { target; error })
      )
  in
  loop targets

let client_scope = function
  | Runtime -> Client.Runtime
  | Dev -> Client.Dev

let client_target = fun packages ->
  match packages with
  | [] -> Client.BuildAll
  | [ package ] -> Client.BuildPackage package
  | packages -> Client.BuildPackages packages

let sort_uniq_strings = fun values ->
  let rec dedupe acc = function
    | [] -> List.rev acc
    | [ value ] -> List.rev (value :: acc)
    | left :: ((right :: _) as rest) ->
        if String.equal left right then
          dedupe acc rest
        else
          dedupe (left :: acc) rest
  in
  values |> List.sort String.compare |> dedupe []

let referenced_hashes_of_artifact = fun (artifact: Riot_store.Artifact.t) ->
  Std.Crypto.Digest.hex artifact.hash
  :: List.map
    (fun (entry: Riot_store.Manifest.export_entry) -> entry.action_hash)
    artifact.exports
  |> sort_uniq_strings

let generation_lane_of_results = fun ~profile ~target results ->
  let hashes =
    List.concat_map
      (fun (result: Riot_executor.Package_builder.build_result) ->
        match result.status with
        | Riot_executor.Package_builder.Built artifact
        | Riot_executor.Package_builder.Cached artifact ->
            referenced_hashes_of_artifact artifact
        | Riot_executor.Package_builder.Skipped _
        | Riot_executor.Package_builder.Failed _ -> [])
      results
    |> sort_uniq_strings
  in
  Riot_store.Cache_gc.{ profile; target; hashes }

let new_entries_of_results = fun ~profile ~target results ->
  List.filter_map
    (fun (result: Riot_executor.Package_builder.build_result) ->
      match result.status with
      | Riot_executor.Package_builder.Built artifact ->
          Some Riot_store.Cache_gc.{
            profile;
            target;
            hash = Std.Crypto.Digest.hex artifact.Riot_store.Artifact.hash;
          }
      | Riot_executor.Package_builder.Cached _
      | Riot_executor.Package_builder.Skipped _
      | Riot_executor.Package_builder.Failed _ -> None)
    results

let record_successful_build_cache_generation = fun request lane_results ->
  let lanes =
    List.map
      (fun (target, results) ->
        generation_lane_of_results ~profile:request.profile ~target results)
      lane_results
  in
  let new_entries =
    List.map
      (fun (target, results) ->
        new_entries_of_results ~profile:request.profile ~target results)
      lane_results
    |> List.concat
  in
  match Riot_store.Cache_gc.record_successful_build
    ~workspace:request.workspace
    ~lanes
    ~new_entries with
  | Ok _ -> Ok ()
  | Error _ -> Ok ()

let build_with_connect = fun connect ?(record_cache_generation = true) ?(on_event = no_event) ?workspace_manager request ->
  match resolve_targets request with
  | Error _ as err -> err
  | Ok targets -> (
      match ensure_toolchains_for_targets request.workspace targets with
      | Error _ as err -> err
      | Ok () -> (
          match validate_target_toolchains request.workspace targets with
          | Error _ as err -> err
          | Ok () -> (
              match connect ?workspace_manager ~workspace:request.workspace () with
              | Error err -> Error (ClientError err)
              | Ok client ->
                  try
                    let host = Riot_toolchain.get_host_triple () in
                    let request_target = client_target request.packages in
                    let rec loop acc = function
                      | [] -> Ok (List.rev acc)
                      | target :: rest ->
                          on_event (BuildingTarget { target; host = String.equal target host });
                          let target_arch =
                            if String.equal target host then
                              None
                            else
                              Some target
                          in
                          match Client.build_streaming
                            client
                            request_target
                            ~scope:(client_scope request.scope)
                            ~profile:request.profile
                            ?target_arch
                            (fun event -> on_event (Streaming event)) with
                          | Ok (Client.BuildCompleted { results; _ }) -> loop ((target, results) :: acc) rest
                          | Ok _ -> loop acc rest
                          | Error err -> Error (ClientError err)
                    in
                    let result = loop [] targets in
                    Client.close client;
                    (
                      match result with
                      | Ok lane_results ->
                          let _ =
                            if record_cache_generation then
                              record_successful_build_cache_generation request lane_results
                            else
                              Ok ()
                          in
                          Ok (List.map snd lane_results |> List.concat)
                      | Error _ as err -> err
                    )
                  with
                  | exn ->
                      Client.close client;
                      raise exn
            )
        )
    )

let build = fun ?(record_cache_generation = true) ?(on_event = no_event) ?workspace_manager request ->
  let pm_session_id = Riot_model.Session_id.make () in
  build_with_connect
    ~record_cache_generation
    (fun ?workspace_manager ~workspace () ->
      Client.connect_local
        ?workspace_manager
        ~emit:(fun kind ->
          on_event
            (Pm (Riot_model.Event.create ~session_id:pm_session_id ~level:Riot_model.Event.Info kind)))
        ~workspace
        ())
    ~on_event
    ?workspace_manager
    request

let build_prepared = fun ?(record_cache_generation = true) ?(on_event = no_event) ?workspace_manager request ->
  build_with_connect
    ~record_cache_generation
    (fun ?workspace_manager ~workspace () ->
      Client.connect_local_prepared ?workspace_manager ~workspace ())
    ~on_event
    ?workspace_manager
    request
