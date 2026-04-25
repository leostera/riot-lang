open Std
open Riot_model
open Std.Result.Syntax

type publish_selection =
  | Workspace
  | Package of Package_name.t

type publish_request = { selection: publish_selection; skip_check: bool }

type publish_mode =
  | DryRun
  | Publish

type publish_check_stage = [ | `fmt | `fix | `build | `metadata]

type publish_event =
  | Fmt of Riot_fmt.event
  | Fix of Riot_fix.Event.t
  | Build of Riot_build.Event.t
  | CheckStarted of {
    package: Package_name.t;
    version: Std.Version.t option;
    stage: publish_check_stage;
  }
  | CheckFinished of {
    package: Package_name.t;
    version: Std.Version.t option;
    stage: publish_check_stage;
  }
  | Packing of { package: Package_name.t; version: Std.Version.t; artifact_path: Path.t }
  | SkippedNotPublic of { package: Package_name.t; version: Std.Version.t option }
  | SkippedAlreadyPublished of { package: Package_name.t; version: Std.Version.t }
  | DryRunPlanned of Riot_deps.Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release

type publish_outcome =
  | SkippedNotPublicPackage of { package: Package_name.t; version: Std.Version.t option }
  | Skipped of { package: Package_name.t; version: Std.Version.t }
  | Planned of Riot_deps.Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release

type publish_error =
  | PackageNotFound of { package: Package_name.t }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Riot_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of {
    registry_name: string;
    error: Riot_deps.registry_initialization_error;
  }
  | WorkspaceScanFailed of {
    workspace_root: Path.t;
    error: Riot_model.Workspace_manager.scan_error;
  }
  | WorkspaceLoadHadErrors of {
    workspace_root: Path.t;
    errors: Riot_model.Workspace_manager.load_error list;
  }
  | WorkspacePrepareFailed of { workspace_root: Path.t; error: Riot_model.Pm_error.t }
  | FmtCheckFailed of { package: Package_name.t; error: exn }
  | FixCheckFailed of { package: Package_name.t; error: exn }
  | BuildCheckFailed of { package: Package_name.t; error: Riot_build.error }
  | PublishPlanFailed of Riot_deps.Publisher.error
  | PublishFailed of { package: Package_name.t; error: Riot_deps.Publisher.error }

let default_registry_name = "pkgs.ml"

let no_event: publish_event -> unit = fun _ -> ()

let exn_message = fun exn ->
  match exn with
  | Failure message -> message
  | exn -> Exception.to_string exn

let registry_initialization_error_message = function
  | Riot_deps.RegistryFilesystemInitializationFailed error -> Pkgs_ml.Registry_cache.create_error_message error

let publish_error_message = fun error ->
  match error with
  | PackageNotFound { package } -> "package '" ^ Package_name.to_string package ^ "' was not found in this workspace"
  | NoWorkspacePackages -> "no workspace packages were found to publish"
  | PublishConfigLoadFailed err -> Riot_model.User_config.message err
  | MissingApiToken { registry_name; path } -> "missing API token for registry '" ^ registry_name ^ "' in " ^ Path.to_string path ^ " (expected [registry.\"" ^ registry_name ^ "\"].api_token)"
  | RegistryInitializationFailed { registry_name; error } -> "failed to initialize registry '" ^ registry_name ^ "': " ^ registry_initialization_error_message error
  | WorkspaceScanFailed { workspace_root; error } -> "failed to scan workspace '" ^ Path.to_string workspace_root ^ "': " ^ Workspace_manager.scan_error_message error
  | WorkspaceLoadHadErrors { workspace_root; errors } -> "failed to load workspace '" ^ Path.to_string workspace_root ^ "': " ^ (errors |> List.map ~fn:Workspace_manager.load_error_to_string |> String.concat "\n")
  | WorkspacePrepareFailed { workspace_root; error } -> "failed to prepare workspace '" ^ Path.to_string workspace_root ^ "': " ^ Riot_model.Pm_error.message error
  | FmtCheckFailed { package; error } -> "'riot fmt --check' failed for package '" ^ Package_name.to_string package ^ "': " ^ exn_message error
  | FixCheckFailed { package; error } -> "'riot fix --check' failed for package '" ^ Package_name.to_string package ^ "': " ^ exn_message error
  | BuildCheckFailed { package; error } -> "'riot build' failed for package '" ^ Package_name.to_string package ^ "': " ^ Riot_build.error_message error
  | PublishPlanFailed err -> Riot_deps.Publisher.message err
  | PublishFailed { error; _ } -> Riot_deps.Publisher.message error

let publish_error_is_already_published = fun error ->
  match error with
  | Riot_deps.Publisher.RegistryPublishFailed { error; _ } -> String.starts_with ~prefix:"Package " error && String.ends_with ~suffix:" is already published." error
  | _ -> false

let workspace_packages = fun (workspace: Workspace.t) -> Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Runtime workspace |> List.filter ~fn:Package.is_workspace_member

let is_public_package = fun (package: Package.t) ->
  match package.publish.is_public with
  | Some true -> true
  | Some false | None -> false

let select_packages = fun ~(workspace_publish_order:packages:Package.t list -> (Package.t list, publish_error) result) ~(emit:publish_event -> unit) ~(workspace:Workspace.t) request ->
  let packages = workspace_packages workspace in
  let public_packages = List.filter packages ~fn:is_public_package in
  match request.selection with
  | Package package_name -> (
    match List.find packages ~fn:(
      fun (pkg: Package.t) -> Package_name.equal pkg.name package_name
    ) with
    | Some pkg when not (is_public_package pkg) ->
        emit (SkippedNotPublic { package = pkg.name; version = pkg.publish.version });
        Ok []
    | Some pkg -> Ok [ pkg ]
    | None -> Error (PackageNotFound { package = package_name })
  )
  | Workspace ->
      if packages = [] then
        Error NoWorkspacePackages
      else
        if public_packages = [] then
          Ok []
        else workspace_publish_order ~packages:public_packages

let load_api_token = fun ~registry_name ->
  let config_path = Riot_model.Riot_dirs.config_path () in
  match Fs.exists config_path with
  | Error io_error -> Error (PublishConfigLoadFailed (Riot_model.User_config.ReadFailed { path = config_path; error = io_error }))
  | Ok false -> Error (MissingApiToken { registry_name; path = config_path })
  | Ok true -> (
    match Riot_model.User_config.load config_path with
    | Error err -> Error (PublishConfigLoadFailed err)
    | Ok config -> (
      match Riot_model.User_config.api_token config ~registry_name with
      | Some token -> Ok token
      | None -> Error (MissingApiToken { registry_name; path = config_path })
    )
  )

let resolve_registry = fun ?(registry_name = default_registry_name) () -> Pkgs_ml.Registry.create_filesystem ~registry_name () |> Result.map_err ~fn:(
  fun error -> RegistryInitializationFailed { registry_name; error = Riot_deps.RegistryFilesystemInitializationFailed error }
)

let run_check = fun ~emit ~package_name ~version ~stage check_fn ->
  emit (CheckStarted { package = package_name; version; stage });
  let* value = check_fn ()
  in
  emit (CheckFinished { package = package_name; version; stage });
  Ok value

let load_workspace_strict = fun workspace_root ->
  let workspace_manager = Workspace_manager.create () in
  let* registry = resolve_registry ()
  in
  match Workspace_manager.scan workspace_manager workspace_root with
  | Error error -> Error (WorkspaceScanFailed { workspace_root; error })
  | Ok (workspace, load_errors) ->
      if List.is_empty load_errors then
        Riot_deps.ensure_workspace ~workspace_manager ~mode:Riot_deps.Dep_solver.Refresh ~registry ~workspace () |> Result.map_err ~fn:(
          fun error -> WorkspacePrepareFailed { workspace_root; error }
        )
      else Error (WorkspaceLoadHadErrors { workspace_root; errors = load_errors })

let profile_of_name = function
  | "release" -> Riot_model.Profile.release
  | _ -> Riot_model.Profile.debug

let build_package = fun ~emit ~(workspace:Workspace.t) ~package_name ~profile -> Riot_build.build ~on_event:(
  fun event -> emit (Build event)
) (Riot_build.Request.make ~workspace ~packages:[ package_name ] ~targets:Riot_model.Target.Host ~scope:Riot_build.Request.Runtime ~profile:(profile_of_name profile) ()) |> Result.map_err ~fn:(
  fun error -> BuildCheckFailed { package = package_name; error }
)

let build_package_in_workspace_root = fun ~emit ~workspace_root ~package_name ~profile ->
  let* workspace = load_workspace_strict workspace_root in build_package ~emit ~workspace ~package_name ~profile

let fix_request_for_publish = fun ~cwd ~target ->
  let request = Riot_fix.check_request ~cwd ~target in
  match request.action with
  | Riot_fix.Run { mode; limit; target; output_mode = _; use_generated_runner } ->
      let output_mode =
        if use_generated_runner then
          Riot_fix.Report Riot_fix.Reporter.Text
        else Riot_fix.Silent
      in
      {
        request with
        action = Riot_fix.Run {
          mode;
          limit;
          target;
          output_mode;
          use_generated_runner
        }
      }
  | _ -> request

module For_test = struct
  type deps = {
    resolve_registry: unit -> (Pkgs_ml.Registry.t, publish_error) result;
    load_api_token: registry_name:string -> (string, publish_error) result;
    workspace_publish_order: packages:Riot_model.Package.t list -> (Riot_model.Package.t list, publish_error) result;
    published_version_exists: registry:Pkgs_ml.Registry.t -> package_name:Riot_model.Package_name.t -> version:Std.Version.t -> (bool, publish_error) result;
    run_fmt_check: emit:(publish_event -> unit) -> workspace:Riot_model.Workspace.t -> package:Riot_model.Package.t -> (unit, publish_error) result;
    run_fix_check: emit:(publish_event -> unit) -> registry:Pkgs_ml.Registry.t -> workspace:Riot_model.Workspace.t -> request:publish_request -> package:Riot_model.Package.t -> (unit, publish_error) result;
    run_build_check: emit:(publish_event -> unit) -> workspace:Riot_model.Workspace.t -> package_name:Riot_model.Package_name.t -> profile:string -> (unit, publish_error) result;
    plan_publish: registry:Pkgs_ml.Registry.t -> publishing_workspace_packages:Riot_model.Package_name.t list -> package:Riot_model.Package.t -> (Riot_deps.Publisher.publish_plan, publish_error) result;
    prepare_publish_artifact: target_dir_root:Path.t -> Riot_deps.Publisher.publish_plan -> (Riot_deps.Publisher.prepared_publish, publish_error) result;
    publish_prepared: registry:Pkgs_ml.Registry.t -> api_token:string -> Riot_deps.Publisher.prepared_publish -> (Pkgs_ml.Registry.published_release, publish_error) result;
  }

  let run_fmt_check_default = fun ~emit ~workspace ~package ->
    let package: Package.t = package in Riot_fmt.run_check_paths ~workspace ~on_event:(
      fun event -> emit (Fmt event)
    ) [ package.path ] |> Result.map_err ~fn:(
      fun error -> FmtCheckFailed { package = package.name; error }
    )

  let run_fix_check_default = fun ~emit ~registry ~workspace ~request:_ ~package ->
    let workspace: Workspace.t = workspace in
    let package: Package.t = package in
    let fix_request = fix_request_for_publish ~cwd:workspace.root ~target:package.path in Riot_fix.fix ~on_event:(
      fun event -> emit (Fix event)
    ) ~build_package:(
      fun ~(workspace:Workspace_manifest.t) ~package_name ~profile ?(transform_workspace = fun workspace -> workspace) () ->
        let workspace_manager = Workspace_manager.create () in
        match Riot_deps.ensure_workspace ~workspace_manager ~mode:Riot_deps.Dep_solver.Refresh ~registry ~workspace () with
        | Error err -> Error (Failure (Riot_model.Pm_error.message err))
        | Ok workspace -> Riot_build.build ~on_event:(
          fun event -> emit (Build event)
        ) (Riot_build.Request.make ~workspace:(transform_workspace workspace) ~packages:[ package_name ] ~targets:Riot_model.Target.Host ~scope:Riot_build.Request.Runtime ~profile:(profile_of_name profile) ()) |> Result.map ~fn:(
          fun _ -> ()
        ) |> Result.map_err ~fn:(
          fun err -> Failure (Riot_build.error_message err)
        )
    ) fix_request |> Result.map ~fn:(
      fun _ -> ()
    ) |> Result.map_err ~fn:(
      fun error -> FixCheckFailed { package = package.name; error }
    )

  let run_build_check_default = fun ~emit ~(workspace:Workspace.t) ~package_name ~profile -> build_package ~emit ~workspace ~package_name ~profile |> Result.map ~fn:(
    fun _ -> ()
  )

  let workspace_publish_order_default = fun ~packages -> Riot_deps.Publisher.workspace_publish_order ~packages |> Result.map_err ~fn:(
    fun err -> PublishPlanFailed err
  )

  let plan_publish_default = fun ~registry ~publishing_workspace_packages ~package -> Riot_deps.Publisher.plan_publish ~registry ~publishing_workspace_packages ~package |> Result.map_err ~fn:(
    fun err -> PublishPlanFailed err
  )

  let prepare_publish_artifact_default = fun ~target_dir_root plan -> Riot_deps.Publisher.prepare_publish_artifact ~target_dir_root plan |> Result.map_err ~fn:(
    fun err -> PublishPlanFailed err
  )

  let publish_prepared_default = fun ~registry ~api_token prepared -> Riot_deps.Publisher.publish_prepared ~registry ~api_token prepared |> Result.map_err ~fn:(
    fun err -> PublishFailed { package = prepared.package.name; error = err }
  )

  let default_deps = {
    resolve_registry;
    load_api_token;
    workspace_publish_order = workspace_publish_order_default;
    published_version_exists = (
      fun ~registry ~package_name ~version -> Riot_deps.Publisher.published_version_exists ~registry ~package_name ~version |> Result.map_err ~fn:(
        fun err -> PublishPlanFailed err
      )
    );
    run_fmt_check = run_fmt_check_default;
    run_fix_check = run_fix_check_default;
    run_build_check = run_build_check_default;
    plan_publish = plan_publish_default;
    prepare_publish_artifact = prepare_publish_artifact_default;
    publish_prepared = publish_prepared_default
  }

  let run_publish_checks = fun ~deps ~emit ~registry ~(workspace:Workspace.t) ~request ~publishing_workspace_packages (package: Package.t) ->
    let* () = run_check ~emit ~package_name:package.name ~version:package.publish.version ~stage:`fmt (
      fun () -> deps.run_fmt_check ~emit ~workspace ~package
    )
    in
    let* () =
      if request.skip_check then
        Ok ()
      else run_check ~emit ~package_name:package.name ~version:package.publish.version ~stage:`fix (
        fun () -> deps.run_fix_check ~emit ~registry ~workspace ~request ~package
      )
    in
    let* () = run_check ~emit ~package_name:package.name ~version:package.publish.version ~stage:`build (
      fun () -> deps.run_build_check ~emit ~workspace ~package_name:package.name ~profile:"release"
    ) in run_check ~emit ~package_name:package.name ~version:package.publish.version ~stage:`metadata (
      fun () -> deps.plan_publish ~registry ~publishing_workspace_packages ~package
    )

  let rec run_packages = fun ~deps ~(emit:publish_event -> unit) ~registry ~(workspace:Workspace.t) ~request ~publishing_workspace_packages ~api_token_opt ~mode acc packages ->
    match packages with
    | [] -> Ok (List.reverse acc)
    | (package: Package.t) :: rest ->
        let* already_published =
          match package.publish.version with
          | Some version -> deps.published_version_exists ~registry ~package_name:package.name ~version
          | None -> Ok false
        in
        if already_published then
          let version = Option.unwrap package.publish.version in
          let event: publish_event = SkippedAlreadyPublished { package = package.name; version } in
          let outcome: publish_outcome = Skipped { package = package.name; version } in emit event;
          run_packages ~deps ~emit ~registry ~workspace ~request ~publishing_workspace_packages ~api_token_opt ~mode (outcome :: acc) rest
        else
          let* plan = run_publish_checks ~deps ~emit ~registry ~workspace ~request ~publishing_workspace_packages package
          in
          let* prepared = deps.prepare_publish_artifact ~target_dir_root:workspace.target_dir_root plan in emit (Packing { package = package.name; version = prepared.version; artifact_path = prepared.artifact_path });
        match mode, api_token_opt with
        | DryRun, _ ->
            emit (DryRunPlanned prepared);
            run_packages ~deps ~emit ~registry ~workspace ~request ~publishing_workspace_packages ~api_token_opt ~mode (Planned prepared :: acc) rest
        | Publish, Some api_token -> (
          match deps.publish_prepared ~registry ~api_token prepared with
          | Error (PublishFailed { error; _ }) when publish_error_is_already_published error ->
              let event: publish_event = SkippedAlreadyPublished { package = package.name; version = prepared.version } in
              let outcome: publish_outcome = Skipped { package = package.name; version = prepared.version } in
              emit event;
              run_packages ~deps ~emit ~registry ~workspace ~request ~publishing_workspace_packages ~api_token_opt ~mode (outcome :: acc) rest
          | Error err -> Error err
          | Ok published ->
              emit (PackagePublished published);
              run_packages ~deps ~emit ~registry ~workspace ~request ~publishing_workspace_packages ~api_token_opt ~mode (Published published :: acc) rest
        )
        | Publish, None -> panic "expected API token in publish mode"

  let publish_with = fun ?(on_event = no_event) ~deps ~(workspace:Workspace.t) ~request ~mode () ->
    let* registry = deps.resolve_registry ()
    in
    let* packages = select_packages ~workspace_publish_order:deps.workspace_publish_order ~emit:on_event ~workspace request
    in
    let publishing_workspace_packages = List.map packages ~fn:(
      fun (pkg: Package.t) -> pkg.name
    ) in
    let* api_token_opt =
      match mode with
      | DryRun -> Ok None
      | Publish -> deps.load_api_token ~registry_name:(Pkgs_ml.Registry.name registry) |> Result.map ~fn:(
        fun token -> Some token
      )
    in
    run_packages ~deps ~emit:on_event ~registry ~workspace ~request ~publishing_workspace_packages ~api_token_opt ~mode [] packages
end

let publish = fun ?(on_event = no_event) ~(workspace:Workspace.t) ~request ~mode () -> For_test.publish_with ~on_event ~deps:For_test.default_deps ~workspace ~request ~mode ()
