open Std
open Riot_model

type publish_selection =
  | Workspace
  | Package of string

type publish_request = {
  selection: publish_selection;
  skip_check: bool;
}

type publish_mode =
  | DryRun
  | Publish

type publish_check_stage =
[
  | `fmt
  | `fix
  | `build
  | `metadata
]

type publish_event =
  | Fmt of Krasny.Report.event
  | Fix of Riot_fix.Event.t
  | Build of Riot_build.build_event
  | CheckStarted of { package: string; version: Std.Version.t option; stage: publish_check_stage }
  | CheckFinished of { package: string; version: Std.Version.t option; stage: publish_check_stage }
  | Packing of { package: string; version: Std.Version.t; artifact_path: Path.t }
  | SkippedNotPublic of { package: string; version: Std.Version.t option }
  | SkippedAlreadyPublished of { package: string; version: Std.Version.t }
  | DryRunPlanned of Riot_deps.Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release

type publish_outcome =
  | SkippedNotPublicPackage of { package: string; version: Std.Version.t option }
  | Skipped of { package: string; version: Std.Version.t }
  | Planned of Riot_deps.Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release

type publish_error =
  | PackageNotFound of { package: string }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Riot_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | WorkspaceScanFailed of { workspace_root: Path.t; error: string }
  | FmtCheckFailed of { package: string; error: string }
  | FixCheckFailed of { package: string; error: string }
  | BuildCheckFailed of { package: string; error: string }
  | PublishPlanFailed of Riot_deps.Publisher.error
  | PublishFailed of { package: string; error: Riot_deps.Publisher.error }

let default_registry_name = "pkgs.ml"

let no_event: publish_event -> unit = fun _ -> ()

let ( let* ) = Result.and_then

let exn_message = fun exn ->
  match exn with
  | Failure message -> message
  | exn -> Exception.to_string exn

let publish_error_message = fun error ->
  match error with
  | PackageNotFound { package } -> "package '" ^ package ^ "' was not found in this workspace"
  | NoWorkspacePackages -> "no workspace packages were found to publish"
  | PublishConfigLoadFailed err -> Riot_model.User_config.message err
  | MissingApiToken { registry_name; path } -> "missing API token for registry '"
  ^ registry_name
  ^ "' in "
  ^ Path.to_string path
  ^ " (expected [registry.\""
  ^ registry_name
  ^ "\"].api_token)"
  | RegistryInitializationFailed { registry_name; error } -> "failed to initialize registry '"
  ^ registry_name
  ^ "': "
  ^ error
  | WorkspaceScanFailed { workspace_root; error } -> "failed to scan workspace '"
  ^ Path.to_string workspace_root
  ^ "': "
  ^ error
  | FmtCheckFailed { package; error } -> "'riot fmt --check' failed for package '"
  ^ package
  ^ "': "
  ^ error
  | FixCheckFailed { package; error } -> "'riot fix --check' failed for package '"
  ^ package
  ^ "': "
  ^ error
  | BuildCheckFailed { package; error } -> "'riot build' failed for package '" ^ package ^ "': " ^ error
  | PublishPlanFailed err -> Riot_deps.Publisher.message err
  | PublishFailed { error; _ } -> Riot_deps.Publisher.message error

let publish_error_is_already_published = fun error ->
  match error with
  | Riot_deps.Publisher.RegistryPublishFailed { error; _ } -> String.starts_with ~prefix:"Package " error
  && String.ends_with ~suffix:" is already published." error
  | _ -> false

let workspace_packages = fun (workspace: Workspace.t) -> workspace.packages |> List.filter Package.is_workspace_member

let is_public_package = fun (package: Package.t) ->
  match package.publish.is_public with
  | Some true -> true
  | Some false
  | None -> false

let select_packages = fun ~(emit:publish_event -> unit) ~(workspace:Workspace.t) request ->
  let packages = workspace_packages workspace in
  let public_packages = List.filter is_public_package packages in
  match request.selection with
  | Package package_name -> (
      match
        List.find_opt
          (fun (pkg: Package.t) ->
            String.equal pkg.name package_name)
          packages
      with
      | Some pkg when not (is_public_package pkg) ->
          emit (SkippedNotPublic { package = pkg.name; version = pkg.publish.version });
          Ok []
      | Some pkg ->
          Ok [ pkg ]
      | None ->
          Error (PackageNotFound { package = package_name })
    )
  | Workspace ->
      if packages = [] then
        Error NoWorkspacePackages
      else if public_packages = [] then
        Ok []
      else
        Riot_deps.Publisher.workspace_publish_order ~packages:public_packages
        |> Result.map_error (fun err -> PublishPlanFailed err)

let load_api_token = fun ~registry_name ->
  let config_path = Riot_model.Riot_dirs.config_path () in
  match Fs.exists config_path with
  | Error io_error ->
      Error (PublishConfigLoadFailed (Riot_model.User_config.ReadFailed {
        path = config_path;
        error = IO.error_message io_error
      }))
  | Ok false ->
      Error (MissingApiToken { registry_name; path = config_path })
  | Ok true -> (
      match Riot_model.User_config.load config_path with
      | Error err -> Error (PublishConfigLoadFailed err)
      | Ok config -> (
          match Riot_model.User_config.api_token config ~registry_name with
          | Some token -> Ok token
          | None -> Error (MissingApiToken { registry_name; path = config_path })
        )
    )

let resolve_registry = fun ?(registry_name = default_registry_name) () ->
  Pkgs_ml.Registry.create_filesystem ~registry_name ()
  |> Result.map_error (fun error -> RegistryInitializationFailed { registry_name; error })

let run_check = fun ~emit ~package_name ~version ~stage check_fn ->
  emit (CheckStarted { package = package_name; version; stage });
  let* value = check_fn () in
  emit (CheckFinished { package = package_name; version; stage });
  Ok value

let scan_workspace_strict = fun workspace_root ->
  let workspace_manager = Workspace_manager.create () in
  match Workspace_manager.scan workspace_manager workspace_root with
  | Error error -> Error (WorkspaceScanFailed { workspace_root; error })
  | Ok (workspace, load_errors) ->
      if List.is_empty load_errors then
        Ok workspace
      else
        Error (WorkspaceScanFailed {
          workspace_root;
          error = load_errors |> List.map Workspace_manager.load_error_to_string |> String.concat "\n"
        })

let build_package = fun ~emit ~(workspace:Workspace.t) ~package_name ~profile ->
  Riot_build.build ~on_event:(fun event -> emit (Build event))
    Riot_build.{
      workspace;
      packages = [ package_name ];
      targets = Riot_build.Host;
      scope = Riot_build.Runtime;
      profile;
    } |> Result.map_error
    (fun err ->
      BuildCheckFailed { package = package_name; error = Riot_build.build_error_message err })

let build_package_in_workspace_root = fun ~emit ~workspace_root ~package_name ~profile ->
  let* workspace = scan_workspace_strict workspace_root in
  build_package ~emit ~workspace ~package_name ~profile

let fix_request_for_publish = fun ~cwd ~target ->
  let request = Riot_fix.check_request ~cwd ~target in
  match request.action with
  | Riot_fix.Run {
    mode;
    limit;
    target;
    output_mode=_;
    use_generated_runner;

  } ->
      let output_mode =
        if use_generated_runner then
          Riot_fix.Report Riot_fix.Reporter.Text
        else
          Riot_fix.Silent
      in
      {
        request
        with action =
          Riot_fix.Run {
            mode;
            limit;
            target;
            output_mode;
            use_generated_runner;
          };
      }
  | _ -> request

let run_publish_checks = fun ~emit ~registry ~(workspace:Workspace.t) ~request ~publishing_workspace_packages (
  package: Package.t
) ->
  let* () = run_check
    ~emit
    ~package_name:package.name
    ~version:package.publish.version
    ~stage:`fmt
    (fun () ->
      Riot_fmt.run_check_paths ~workspace ~on_event:(fun event -> emit (Fmt event)) [ package.path ])
  |> Result.map_error (fun exn -> FmtCheckFailed { package = package.name; error = exn_message exn }) in
  let fix_request = fix_request_for_publish ~cwd:workspace.root ~target:package.path in
  let* () =
    if request.skip_check then
      Ok ()
    else
      run_check
        ~emit
        ~package_name:package.name
        ~version:package.publish.version
        ~stage:`fix
        (fun () ->
          Riot_fix.fix
            ~on_event:(fun event -> emit (Fix event))
            ~build_package:(fun ~(workspace:Workspace.t) ~package_name ~profile ?(transform_workspace = fun workspace -> workspace) () ->
              match Riot_deps.ensure_workspace ~mode:Riot_deps.Dep_solver.Refresh ~registry ~workspace () with
              | Error err -> Error (Failure (Riot_model.Pm_error.message err))
              | Ok prepared_workspace ->
                  Riot_build.build_prepared ~on_event:(fun event -> emit (Build event))
                    Riot_build.{
                      workspace = transform_workspace prepared_workspace;
                      packages = [ package_name ];
                      targets = Riot_build.Host;
                      scope = Riot_build.Runtime;
                      profile;
                    }
                  |> Result.map (fun _ -> ())
                  |> Result.map_error (fun err -> Failure (Riot_build.build_error_message err)))
            fix_request
          |> Result.map (fun _ -> ()))
      |> Result.map_error
        (fun exn -> FixCheckFailed { package = package.name; error = exn_message exn })
  in
  let* () =
    run_check
      ~emit
      ~package_name:package.name
      ~version:package.publish.version
      ~stage:`build
      (fun () ->
        build_package ~emit ~workspace ~package_name:package.name ~profile:"release"
        |> Result.map (fun _ -> ()))
  in
  run_check
    ~emit
    ~package_name:package.name
    ~version:package.publish.version
    ~stage:`metadata
    (fun () -> Riot_deps.Publisher.plan_publish ~registry ~publishing_workspace_packages ~package)
  |> Result.map_error (fun err -> PublishPlanFailed err)

let rec run_packages = fun ~(emit:publish_event -> unit) ~registry ~(workspace:Workspace.t) ~request ~publishing_workspace_packages ~api_token_opt ~mode acc packages ->
  match packages with
  | [] -> Ok (List.rev acc)
  | (package: Package.t) :: rest ->
      let* already_published =
        match package.publish.version with
        | Some version -> Riot_deps.Publisher.published_version_exists
          ~registry
          ~package_name:package.name
          ~version
        |> Result.map_error (fun err -> PublishPlanFailed err)
        | None -> Ok false
      in
      if already_published then
        let version = Option.unwrap package.publish.version in
        let event: publish_event = SkippedAlreadyPublished { package = package.name; version } in
        let outcome: publish_outcome = Skipped { package = package.name; version } in
        emit event;
        run_packages
          ~emit
          ~registry
          ~workspace
          ~request
          ~publishing_workspace_packages
          ~api_token_opt
          ~mode
          (outcome :: acc)
          rest
      else
        let* plan = run_publish_checks
          ~emit
          ~registry
          ~workspace
          ~request
          ~publishing_workspace_packages
          package in
        let* prepared = Riot_deps.Publisher.prepare_publish_artifact
          ~target_dir_root:workspace.target_dir_root
          plan
        |> Result.map_error (fun err -> PublishPlanFailed err) in
        emit
          (Packing {
            package = package.name;
            version = prepared.version;
            artifact_path = prepared.artifact_path
          });
        match mode, api_token_opt with
        | DryRun, _ ->
            emit (DryRunPlanned prepared);
            run_packages
              ~emit
              ~registry
              ~workspace
              ~request
              ~publishing_workspace_packages
              ~api_token_opt
              ~mode
              (Planned prepared :: acc)
              rest
        | Publish, Some api_token -> (
            match Riot_deps.Publisher.publish_prepared ~registry ~api_token prepared with
            | Error err when publish_error_is_already_published err ->
                let event: publish_event = SkippedAlreadyPublished {
                  package = package.name;
                  version = prepared.version
                } in
                let outcome: publish_outcome = Skipped {
                  package = package.name;
                  version = prepared.version
                } in
                emit event;
                run_packages
                  ~emit
                  ~registry
                  ~workspace
                  ~request
                  ~publishing_workspace_packages
                  ~api_token_opt
                  ~mode
                  (outcome :: acc)
                  rest
            | Error err ->
                Error (PublishFailed { package = package.name; error = err })
            | Ok published ->
                emit (PackagePublished published);
                run_packages
                  ~emit
                  ~registry
                  ~workspace
                  ~request
                  ~publishing_workspace_packages
                  ~api_token_opt
                  ~mode
                  (Published published :: acc)
                  rest
          )
        | Publish, None ->
            panic "expected API token in publish mode"

let publish = fun ?(on_event = no_event) ~(workspace:Workspace.t) ~request ~mode () ->
  let* registry = resolve_registry () in
  let* packages = select_packages ~emit:on_event ~workspace request in
  let publishing_workspace_packages =
    List.map (fun (pkg: Package.t) -> pkg.name) packages
  in
  let* api_token_opt =
    match mode with
    | DryRun -> Ok None
    | Publish -> load_api_token ~registry_name:(Pkgs_ml.Registry.name registry)
    |> Result.map (fun token -> Some token)
  in
  run_packages
    ~emit:on_event
    ~registry
    ~workspace
    ~request
    ~publishing_workspace_packages
    ~api_token_opt
    ~mode
    []
    packages
