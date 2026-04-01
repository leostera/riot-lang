open Std

type run_request = {
  workspace: Tusk_model.Workspace.t;
  load_errors: Tusk_model.Workspace_manager.load_error list;
  current_dir: Path.t;
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

let no_event : run_event -> unit = fun _ -> ()

let build_scope_for_binary = fun (workspace: Tusk_model.Workspace.t) ~package_name ~binary_name ->
  match
    List.find_opt
      (fun (pkg: Tusk_model.Package.t) ->
        String.equal pkg.name package_name)
      workspace.packages
  with
  | None -> Build_runtime.Runtime
  | Some pkg -> (
      match Tusk_model.Package.scope_of_binary_name pkg ~binary_name with
      | Some Tusk_model.Package.Dev -> Build_runtime.Dev
      | Some Tusk_model.Package.Normal
      | Some Tusk_model.Package.Build
      | None -> Build_runtime.Runtime
    )

let run_error_message = function
  | BinaryNotFound { binary_name } ->
      "binary '" ^ binary_name ^ "' not found"
  | BinaryNotFoundInPackage { package_name; binary_name } ->
      "binary '" ^ binary_name ^ "' not found in package '" ^ package_name ^ "'"
  | BuildFailed err ->
      Build_runtime.error_message err
  | ArtifactNotFound { reason; _ } ->
      reason
  | ProcessExited code ->
      "process exited with " ^ Int.to_string code
  | SystemError msg ->
      msg
  | ClientError err ->
      Client.error_message err

let run_event_to_json = function
  | Build event ->
      Event.to_json event
  | RunningBinary { package; binary; args } ->
      Some
        (Data.Json.Object [
          ("type", Data.Json.String "RunningBinary");
          ("package", Data.Json.String package);
          ("binary", Data.Json.String binary);
          ("args", Data.Json.Array (List.map Data.Json.string args));
        ])

let reconnect = fun ~workspace ->
  Client.connect_local ~workspace () |> Result.map_error (fun err -> ClientError err)

let run = fun ?(on_event = no_event) (request: run_request) ->
  match reconnect ~workspace:request.workspace with
  | Error _ as err -> err
  | Ok client ->
      let result =
        match Client.scan_workspace client ~current_dir:request.current_dir with
        | Error reason ->
            Error (SystemError reason)
        | Ok () -> (
            match Client.find_executable client request.binary_name with
            | Error reason ->
                Error (SystemError reason)
            | Ok None ->
                Error (BinaryNotFound { binary_name = request.binary_name })
            | Ok (Some (package_name, _binary)) -> (
                match request.package_name with
                | Some expected_package when not (String.equal expected_package package_name) ->
                    Error
                      (BinaryNotFoundInPackage {
                        package_name = expected_package;
                        binary_name = request.binary_name;
                      })
                | _ -> (
                    let scope =
                      build_scope_for_binary
                        request.workspace
                        ~package_name
                        ~binary_name:request.binary_name
                    in
                    match
                      Build_runtime.build
                        ~on_event:(fun event -> on_event (Build event))
                        {
                          workspace = request.workspace;
                          load_errors = request.load_errors;
                          packages = [ package_name ];
                          targets = Build_runtime.Host;
                          scope;
                          profile = "debug";
                        }
                    with
                    | Error err ->
                        Error (BuildFailed err)
                    | Ok () -> (
                        match reconnect ~workspace:request.workspace with
                        | Error _ as err -> err
                        | Ok refreshed_client ->
                            let result =
                              match
                                Client.find_artifact
                                  refreshed_client
                                  ~package:package_name
                                  ~kind:"binary"
                                  ~name:request.binary_name
                              with
                              | Error reason ->
                                  Error
                                    (ArtifactNotFound {
                                      package_name;
                                      binary_name = request.binary_name;
                                      reason;
                                    })
                              | Ok path ->
                                  on_event
                                    (RunningBinary {
                                      package = package_name;
                                      binary = request.binary_name;
                                      args = request.args;
                                    });
                                  let cmd = Command.make path ~args:request.args in
                                  (
                                    match Command.status cmd with
                                    | Ok 0 -> Ok ()
                                    | Ok code -> Error (ProcessExited code)
                                    | Error (Command.SystemError msg) -> Error (SystemError msg)
                                  )
                            in
                            Client.close refreshed_client;
                            result
                      )
                  )
              )
          )
      in
      Client.close client;
      result
