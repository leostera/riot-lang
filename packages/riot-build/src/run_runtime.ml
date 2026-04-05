open Std

type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: string option;
  binary_name: string;
  args: string list;
}

type run_event =
  | Build of Build_runtime.build_event
  | RunningBinary of { package: string; binary: string; args: string list }

type run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of Build_runtime.build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ProcessExited of int
  | SystemError of string
  | ClientError of Client.error

let no_event: run_event -> unit = fun _ -> ()

let build_scope_for_binary = fun (workspace: Riot_model.Workspace.t) ~package_name ~binary_name ->
  match
    List.find_opt
      (fun (pkg: Riot_model.Package.t) ->
        String.equal pkg.name package_name)
      workspace.packages
  with
  | None -> Build_runtime.Runtime
  | Some pkg -> (
      match Riot_model.Package.scope_of_binary_name pkg ~binary_name with
      | Some Riot_model.Package.Dev -> Build_runtime.Dev
      | Some Riot_model.Package.Normal
      | Some Riot_model.Package.Build
      | None -> Build_runtime.Runtime
    )

let run_error_message = function
  | BinaryNotFound { binary_name } -> "binary '" ^ binary_name ^ "' not found"
  | BinaryNotFoundInPackage { package_name; binary_name } -> "binary '"
  ^ binary_name
  ^ "' not found in package '"
  ^ package_name
  ^ "'"
  | BuildFailed err -> Build_runtime.error_message err
  | ArtifactNotFound { reason; _ } -> reason
  | ProcessExited code -> "process exited with " ^ Int.to_string code
  | SystemError msg -> msg
  | ClientError err -> Client.error_message err

let run_event_to_json = function
  | Build event -> Event.to_json event
  | RunningBinary { package; binary; args } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RunningBinary");
    ("package", Data.Json.String package);
    ("binary", Data.Json.String binary);
    ("args", Data.Json.Array (List.map Data.Json.string args));
  ])

let reconnect = fun ~workspace ->
  Client.connect_local ~workspace () |> Result.map_error (fun err -> ClientError err)

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
      | Some path -> Ok (Path.to_string path)
      | None -> Error (ArtifactNotFound {
        package_name;
        binary_name;
        reason = "binary '" ^ binary_name ^ "' resolved to an invalid absolute export path"
      })
    )

let run = fun ?(on_event = no_event) (request: run_request) ->
  match reconnect ~workspace:request.workspace with
  | Error _ as err -> err
  | Ok client ->
      let result =
        match Client.find_executable client request.binary_name with
        | Error reason ->
            Error (SystemError reason)
        | Ok None ->
            Error (BinaryNotFound { binary_name = request.binary_name })
        | Ok (Some (package_name, _binary)) -> (
            match request.package_name with
            | Some expected_package when not (String.equal expected_package package_name) -> Error (BinaryNotFoundInPackage {
              package_name = expected_package;
              binary_name = request.binary_name
            })
            | _ -> (
                let scope = build_scope_for_binary
                  request.workspace
                  ~package_name
                  ~binary_name:request.binary_name in
                match
                  Build_runtime.build
                    ~record_cache_generation:false
                    ~on_event:(fun event -> on_event (Build event))
                    {
                      workspace = request.workspace;
                      packages = [ package_name ];
                      targets = Build_runtime.Host;
                      scope;
                      profile = "debug";
                    }
                with
                | Error err -> Error (BuildFailed err)
                | Ok results -> (
                    let store = Riot_store.Store.create_for_lane
                      ~workspace:request.workspace
                      ~profile:"debug"
                      ~target:(Riot_model.Riot_dirs.host_target ()) in
                    match find_built_binary_path
                      ~store
                      ~package_name
                      ~binary_name:request.binary_name
                      results with
                    | Error _ as err -> err
                    | Ok path ->
                        on_event
                          (RunningBinary {
                            package = package_name;
                            binary = request.binary_name;
                            args = request.args
                          });
                        let cmd = Command.make path ~args:request.args in
                        (
                          match Command.status cmd with
                          | Ok 0 -> Ok ()
                          | Ok code -> Error (ProcessExited code)
                          | Error (Command.SystemError msg) -> Error (SystemError msg)
                        )
                  )
              )
          )
      in
      Client.close client;
      result
