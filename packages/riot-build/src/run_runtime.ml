open Std

type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: string option;
  binary_name: string;
  profile: string;
  args: string list;
}

type source_run_request = {
  source_spec: string;
  binary_name: string;
  profile: string;
  update: bool;
  args: string list;
}

type runnable_binary = {
  package_name: string;
  binary_name: string;
  source_path: Path.t;
}

type run_event =
  | Build of Event.t
  | RunningBinary of { package: string; binary: string; args: string list }

type run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of Build_core.error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ProcessExited of int
  | SystemError of string
  | ExternalTargetLoadFailed of { target: string; reason: string }
  | ClientError of Client.error

let ( let* ) value fn = Result.and_then value ~fn

let no_event: run_event -> unit = fun _ -> ()

let realized_runnable_packages = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Run workspace
  |> List.filter ~fn:Riot_model.Package.is_workspace_member
  |> List.filter ~fn:(fun (pkg: Riot_model.Package.t) ->
      match package_filter with
      | None -> true
      | Some package_name -> String.equal package_name pkg.name)

let build_scope_for_binary = fun (workspace: Riot_model.Workspace.t) ~package_name ~binary_name ->
  match
    List.find
      (Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Dev workspace)
      ~fn:(fun (pkg: Riot_model.Package.t) ->
      String.equal pkg.name package_name)
  with
  | None -> Request.Runtime
  | Some pkg -> (
      match Riot_model.Package.scope_of_binary_name pkg ~binary_name with
      | Some Riot_model.Package.Dev -> Request.Dev
      | Some Riot_model.Package.Normal
      | Some Riot_model.Package.Build
      | None -> Request.Runtime
    )

let is_listed_runnable = fun (bin: Riot_model.Package.binary) ->
  let path = Path.to_string bin.path in
  (not (String.starts_with ~prefix:"tests/" path)
  && not (String.starts_with ~prefix:"examples/" path)
  && not (String.starts_with ~prefix:"bench/" path))
  || String.starts_with ~prefix:"examples/" path

let list_binaries = fun (workspace: Riot_model.Workspace.t) ?package_filter () ->
  realized_runnable_packages ?package_filter workspace
  |> List.flat_map ~fn:(fun (pkg: Riot_model.Package.t) ->
      pkg.binaries
      |> List.filter ~fn:is_listed_runnable
      |> List.map ~fn:(fun (bin: Riot_model.Package.binary) ->
          {
            package_name = pkg.name;
            binary_name = bin.name;
            source_path =
              Path.(pkg.path / bin.path);
          }))
  |> List.sort ~compare:(fun left right ->
      String.compare
        (left.package_name ^ ":" ^ left.binary_name)
        (right.package_name ^ ":" ^ right.binary_name))

let run_error_message = function
  | BinaryNotFound { binary_name } -> "binary '" ^ binary_name ^ "' not found"
  | BinaryNotFoundInPackage { package_name; binary_name } -> "binary '"
  ^ binary_name
  ^ "' not found in package '"
  ^ package_name
  ^ "'"
  | BuildFailed err -> Build_core.error_message err
  | ArtifactNotFound { reason; _ } -> reason
  | ProcessExited code -> "process exited with " ^ Int.to_string code
  | SystemError msg -> msg
  | ExternalTargetLoadFailed { target; reason } -> "failed to load external target '"
  ^ target
  ^ "': "
  ^ reason
  | ClientError err -> Client.error_message err

let build_event_to_json = fun event ->
  Event.to_json event

let run_event_to_json = function
  | Build event -> build_event_to_json event
  | RunningBinary { package; binary; args } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RunningBinary");
    ("package", Data.Json.String package);
    ("binary", Data.Json.String binary);
    ("args", Data.Json.Array (List.map args ~fn:Data.Json.string));
  ])

let reconnect = fun ~workspace ->
  Client.connect_local ~workspace () |> Result.map_err ~fn:(fun err -> ClientError err)

let make_pm_event = fun session_id kind ->
  Riot_model.Event.create ~session_id ~level:Riot_model.Event.Info kind

let emit_pm_build_event = fun ~session_id ~on_event kind ->
  on_event (Build (Event.Pm (make_pm_event session_id kind)))

let load_source_workspace = fun ~on_event ~source_spec ~update ->
  let session_id = Riot_model.Session_id.make () in
  Riot_deps.load_source_workspace
    ~emit:(emit_pm_build_event ~session_id ~on_event)
    ~update
    ~spec:source_spec
    ()
  |> Result.map_err ~fn:(fun err ->
      ExternalTargetLoadFailed { target = source_spec; reason = Riot_deps.package_error_message err })

let find_built_binary_path = fun ~(store:Riot_store.Store.t) ~(output: Output.t) ~package_name ~binary_name ->
  let ensure_executable_binary_path = fun path ->
    let binary_path = Path.v path in
    match Fs.metadata binary_path with
    | Error err ->
        Error ("failed to read binary metadata: " ^ IO.error_message err)
    | Ok metadata ->
        let mode = Fs.Metadata.mode metadata in
        if mode land 0o111 != 0 then
          Ok path
        else
          Fs.set_permissions binary_path (Fs.Permissions.of_mode (mode lor 0o111))
          |> Result.map ~fn:(fun () -> path)
          |> Result.map_err ~fn:(fun err ->
              "failed to mark binary executable: " ^ IO.error_message err)
  in
  match
    Output.find_package output package_name
    |> Option.and_then ~fn:(fun package_output ->
        Output.find_export package_output binary_name)
  with
  | None -> Error (ArtifactNotFound {
    package_name;
    binary_name;
    reason = "binary '" ^ binary_name ^ "' was not produced by build output"
  })
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path -> ensure_executable_binary_path (Path.to_string path)
      |> Result.map_err ~fn:(fun reason -> ArtifactNotFound { package_name; binary_name; reason })
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
                let prepared_workspace =
                  Prepared_workspace.of_workspace request.workspace
                in
                let build_request =
                  Request.make
                    ~packages:[ package_name ]
                    ~targets:Riot_model.Target.Host
                    ~scope
                    ~profile:(
                      match request.profile with
                      | "release" -> Riot_model.Profile.release
                      | _ -> Riot_model.Profile.debug
                    )
                    ()
                in
                match
                  Build_core.build
                    ~on_event:(fun event -> on_event (Build event))
                    prepared_workspace
                    build_request
                with
                | Error err -> Error (BuildFailed err)
                | Ok output -> (
                    let store = Riot_store.Store.create_for_lane
                      ~workspace:request.workspace
                      ~profile:request.profile
                      ~target:(Riot_model.Riot_dirs.host_target ()) in
                    match find_built_binary_path
                      ~store
                      ~output
                      ~package_name
                      ~binary_name:request.binary_name
                      with
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

let run_source = fun ?(on_event = no_event) (request: source_run_request) ->
  let* loaded = load_source_workspace ~on_event ~source_spec:request.source_spec ~update:request.update in
  run ~on_event
    {
      workspace = loaded.workspace;
      package_name = Some loaded.package_name;
      binary_name = request.binary_name;
      profile = request.profile;
      args = request.args;
    }
