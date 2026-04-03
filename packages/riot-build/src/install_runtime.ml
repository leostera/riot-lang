open Std

type install_request = {
  workspace: Riot_model.Workspace.t;
  binary_name: string;
  local_only: bool;
}

type install_event =
  | Build of Build_runtime.build_event
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; global: bool }
  | PromotionWarning of { binary: string; destination: Path.t; global: bool; reason: string }
  | InstalledBinary of { binary: string; duration_ms: int; global_destination: Path.t option }

type install_error =
  | BinaryNotFound of { binary_name: string }
  | BuildFailed of Build_runtime.build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ClientError of Client.error

let no_event: install_event -> unit = fun _ -> ()

let reconnect = fun ~workspace ->
  Client.connect_local ~workspace () |> Result.map_error (fun err -> ClientError err)

let install_error_message = function
  | BinaryNotFound { binary_name } -> "binary '" ^ binary_name ^ "' not found in workspace"
  | BuildFailed err -> Build_runtime.error_message err
  | ArtifactNotFound { package_name; binary_name; reason } -> "binary '"
  ^ binary_name
  ^ "' was not produced by package '"
  ^ package_name
  ^ "': "
  ^ reason
  | ClientError err -> Client.error_message err

let path_json = fun path -> Data.Json.String (Path.to_string path)

let install_event_to_json = function
  | Build event -> Event.to_json event
  | InstallingBinary { package; binary } -> Some (Data.Json.Object [
    ("type", Data.Json.String "InstallingBinary");
    ("package", Data.Json.String package);
    ("binary", Data.Json.String binary);
  ])
  | PromotedBinary { binary; destination; global } -> Some (Data.Json.Object [
    ("type", Data.Json.String "PromotedBinary");
    ("binary", Data.Json.String binary);
    ("destination", path_json destination);
    ("global", Data.Json.Bool global);
  ])
  | PromotionWarning { binary; destination; global; reason } -> Some (Data.Json.Object [
    ("type", Data.Json.String "PromotionWarning");
    ("binary", Data.Json.String binary);
    ("destination", path_json destination);
    ("global", Data.Json.Bool global);
    ("reason", Data.Json.String reason);
  ])
  | InstalledBinary { binary; duration_ms; global_destination } ->
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "InstalledBinary");
          ("binary", Data.Json.String binary);
          ("duration_ms", Data.Json.Int duration_ms);
          (
            "global_destination",
            match global_destination with
            | Some path -> path_json path
            | None -> Data.Json.Null
          );
        ]
      )

let find_built_binary_path = fun ~(store:Riot_store.Store.t) ~package_name ~binary_name results ->
  let find_binary_export (result: Riot_executor.Package_builder.build_result) =
    if String.equal result.package.name package_name then
      match result.status with
      | Riot_executor.Package_builder.Built artifact
      | Riot_executor.Package_builder.Cached artifact ->
          List.find_opt
            (fun (entry: Riot_store.Manifest.export_entry) ->
              String.equal entry.name binary_name)
            artifact.exports
      | Riot_executor.Package_builder.Skipped _
      | Riot_executor.Package_builder.Failed _ -> None
    else
      None
  in
  match List.find_map find_binary_export results with
  | None -> Error (ArtifactNotFound {
    package_name;
    binary_name;
    reason = "binary '" ^ binary_name ^ "' was not produced by build results"
  })
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path -> Ok path
      | None -> Error (ArtifactNotFound {
        package_name;
        binary_name;
        reason = "binary '" ^ binary_name ^ "' resolved to an invalid absolute export path"
      })
    )

let install_temp_path = fun dst ->
  let dir = Path.dirname dst in
  let name = Path.basename dst in
  let pid = System.OsProcess.current_pid () in
  let nonce = Random.bits () in
  Path.(dir / Path.v ("." ^ name ^ ".install-" ^ Int.to_string pid ^ "-" ^ Int.to_string nonce))

let cleanup_temp_file = fun path ->
  match Fs.remove_file path with
  | Ok () -> ()
  | Error _ -> ()

let install_binary_atomically = fun ~src ~dst ~permissions ->
  let temp_path = install_temp_path dst in
  match Fs.copy ~src ~dst:temp_path with
  | Error err -> Error err
  | Ok () -> (
      match Fs.set_permissions temp_path permissions with
      | Error err ->
          cleanup_temp_file temp_path;
          Error err
      | Ok () -> (
          match Fs.rename ~src:temp_path ~dst with
          | Ok () -> Ok ()
          | Error err ->
              cleanup_temp_file temp_path;
              Error err
        )
    )

let promote_binary = fun ~on_event ~src ~dst ~binary ~global ->
  match install_binary_atomically ~src ~dst ~permissions:Fs.Permissions.executable with
  | Ok () ->
      on_event (PromotedBinary { binary; destination = dst; global });
      Ok true
  | Error reason ->
      on_event
        (PromotionWarning { binary; destination = dst; global; reason = IO.error_message reason });
      Ok false

let install = fun ?(on_event = no_event) (request: install_request) ->
  let started_at = Time.Instant.now () in
  match reconnect ~workspace:request.workspace with
  | Error _ as err -> err
  | Ok client ->
      let result =
        match Client.find_executable client request.binary_name with
        | Error _ as err ->
            err
        | Ok None ->
            Error (BinaryNotFound { binary_name = request.binary_name })
        | Ok (Some (package_name, _binary)) -> (
            on_event (InstallingBinary { package = package_name; binary = request.binary_name });
            match
              Build_runtime.build ~on_event:(fun event -> on_event (Build event))
                {
                  workspace = request.workspace;
                  packages = [ package_name ];
                  targets = Build_runtime.Host;
                  scope = Build_runtime.Runtime;
                  profile = "debug";
                }
            with
            | Error err -> Error (BuildFailed err)
            | Ok results -> (
                let store = Riot_store.Store.create_for_lane
                  ~workspace:request.workspace
                  ~profile:"debug"
                  ~target:(Riot_model.Riot_dirs.host_target ()) in
                match find_built_binary_path ~store ~package_name ~binary_name:request.binary_name results with
                | Error _ as err -> err
                | Ok binary_path ->
                    let workspace_root = request.workspace.root in
                    let project_binary = Path.(workspace_root / Path.v request.binary_name) in
                    let _ = promote_binary
                      ~on_event
                      ~src:binary_path
                      ~dst:project_binary
                      ~binary:request.binary_name
                      ~global:false in
                    let global_destination =
                      if request.local_only then
                        None
                      else
                        let riot_bin_dir = Path.(Riot_model.Riot_dirs.dot_riot / Path.v "bin") in
                        let global_path = Path.(riot_bin_dir / Path.v request.binary_name) in
                        let promoted =
                          match Fs.create_dir_all riot_bin_dir with
                          | Ok () -> promote_binary
                            ~on_event
                            ~src:binary_path
                            ~dst:global_path
                            ~binary:request.binary_name
                            ~global:true
                          | Error reason ->
                              on_event
                                (PromotionWarning {
                                  binary = request.binary_name;
                                  destination = riot_bin_dir;
                                  global = true;
                                  reason = IO.error_message reason
                                });
                              Ok false
                        in
                        match promoted with
                        | Ok true -> Some global_path
                        | Ok false -> None
                        | Error _ -> None
                    in
                    let duration = Time.Instant.duration_since
                      ~earlier:started_at
                      (Time.Instant.now ()) in
                    on_event
                      (InstalledBinary {
                        binary = request.binary_name;
                        duration_ms = Time.Duration.to_millis duration;
                        global_destination
                      });
                    Ok ()
              )
          )
      in
      Client.close client;
      result
