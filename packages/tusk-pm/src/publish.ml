open Std
open Tusk_model

type request =
  | Workspace
  | Package of string

type mode =
  | Dry_run
  | Publish

type check_stage =
[
  | `Fmt
  | `Fix
  | `Build
]

type event =
  | Pm of Tusk_model.Event.kind
  | Fmt of Krasny.Report.event
  | Fix of Tusk_fix.Cli.event
  | CheckStarted of { package: string; stage: check_stage }
  | CheckFinished of { package: string; stage: check_stage }
  | DryRunPlanned of Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release

type outcome =
  | DryRun of Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release

type error =
  | PackageNotFound of { package: string }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Tusk_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | WorkspacePreparationFailed of { error: Tusk_model.Pm_error.t }
  | WorkspaceScanFailed of { workspace_root: Path.t; error: string }
  | ToolchainInitializationFailed of { error: string }
  | FmtCheckFailed of { package: string; error: string }
  | FixCheckFailed of { package: string; error: string }
  | BuildCheckFailed of { package: string; error: string }
  | PublishPlanFailed of Publisher.error
  | PublishFailed of { package: string; error: Publisher.error }

let default_registry_name = "pkgs.ml"

let no_event : event -> unit = fun _ -> ()

let exn_message = function
  | Failure message -> message
  | exn -> Exception.to_string exn

let message = function
  | PackageNotFound { package } -> "package '" ^ package ^ "' was not found in this workspace"
  | NoWorkspacePackages -> "no workspace packages were found to publish"
  | PublishConfigLoadFailed err -> Tusk_model.User_config.message err
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
  | WorkspacePreparationFailed { error } -> Tusk_model.Pm_error.message error
  | WorkspaceScanFailed { workspace_root; error } -> "failed to scan workspace '"
  ^ Path.to_string workspace_root
  ^ "': "
  ^ error
  | ToolchainInitializationFailed { error } -> error
  | FmtCheckFailed { package; error } -> "'tusk fmt --check' failed for package '"
  ^ package
  ^ "': "
  ^ error
  | FixCheckFailed { package; error } -> "'tusk fix --check' failed for package '"
  ^ package
  ^ "': "
  ^ error
  | BuildCheckFailed { package; error } -> "'tusk build' failed for package '" ^ package ^ "': " ^ error
  | PublishPlanFailed err -> Publisher.message err
  | PublishFailed { error; _ } -> Publisher.message error

let workspace_packages = fun (workspace: Workspace.t) -> workspace.packages |> List.filter Package.is_workspace_member

let select_packages = fun ~(workspace:Workspace.t) request ->
  let packages = workspace_packages workspace in
  match request with
  | Package package_name -> (
      match
        List.find_opt
          (fun (pkg: Package.t) ->
            String.equal pkg.name package_name)
          packages
      with
      | Some pkg -> Ok [ pkg ]
      | None -> Error (PackageNotFound { package = package_name })
    )
  | Workspace ->
      if packages = [] then
        Error NoWorkspacePackages
      else
        Publisher.workspace_publish_order ~packages
        |> Result.map_error (fun err -> PublishPlanFailed err)

let load_api_token = fun ~registry_name ->
  let config_path = Tusk_model.Tusk_dirs.config_path () in
  match Fs.exists config_path with
  | Error io_error ->
      Error (PublishConfigLoadFailed (Tusk_model.User_config.ReadFailed {
        path = config_path;
        error = IO.error_message io_error
      }))
  | Ok false ->
      Error (MissingApiToken { registry_name; path = config_path })
  | Ok true -> (
      match Tusk_model.User_config.load config_path with
      | Error err -> Error (PublishConfigLoadFailed err)
      | Ok config -> (
          match Tusk_model.User_config.api_token config ~registry_name with
          | Some token -> Ok token
          | None -> Error (MissingApiToken { registry_name; path = config_path })
        )
    )

let resolve_registry = fun ?(registry_name = default_registry_name) () ->
  Pkgs_ml.Registry.create_filesystem ~registry_name ()
  |> Result.map_error (fun error -> RegistryInitializationFailed { registry_name; error })

let toolchain_for_workspace = fun ~(workspace:Workspace.t) ->
  let toolchain_config = Toolchain_config.from_workspace workspace in
  Tusk_toolchain.init ~config:toolchain_config
  |> Result.map_error (fun error -> ToolchainInitializationFailed { error })

let plan_error_message = function
  | Tusk_planner.Workspace_planner.PackageNotFound { name; available } -> "package '"
  ^ name
  ^ "' was not found (available: "
  ^ String.concat ", " available
  ^ ")"
  | Tusk_planner.Workspace_planner.PackagesNotFound { names; available } -> "packages '"
  ^ String.concat ", " names
  ^ "' were not found (available: "
  ^ String.concat ", " available
  ^ ")"
  | Tusk_planner.Workspace_planner.CycleDetected { cycle } -> "cyclic dependency detected: "
  ^ String.concat " -> " cycle
  | Tusk_planner.Workspace_planner.MissingDependencies { missing } -> missing
  |> List.map
    (fun { Tusk_planner.Package_graph.package; dependency } -> package ^ " requires " ^ dependency)
  |> String.concat "; "
  | Tusk_planner.Workspace_planner.PackageLoadFailed { errors } -> errors
  |> List.map Workspace_manager.load_error_to_string
  |> String.concat "\n"

let build_package_in_workspace = fun ~(workspace:Workspace.t) ~package_name ->
  match toolchain_for_workspace ~workspace with
  | Error _ as err -> err
  | Ok toolchain ->
      let profile = Profile.(apply_overrides debug workspace.profile_overrides) in
      let session_id = Session_id.make () in
      let build_ctx = Build_ctx.make
        ~session_id
        ~profile
        ~available_parallelism:System.available_parallelism
        () in
      let target = Kernel.System.Host.to_string (Build_ctx.target_triplet build_ctx) in
      let store = Tusk_store.Store.create_for_lane ~workspace ~profile:profile.name ~target in
      match Tusk_executor.Coordinator.build_workspace
        ~workspace
        ~toolchain
        ~store
        ~target:(Tusk_planner.Workspace_planner.Package package_name)
        ~scope:Tusk_planner.Package_graph.Runtime
        ~concurrency:System.available_parallelism
        ~build_ctx
        ~session_id with
      | Error err -> Error (BuildCheckFailed {
        package = package_name;
        error = plan_error_message err
      })
      | Ok result ->
          if result.failed_count = 0 then
            Ok ()
          else
            Error (BuildCheckFailed {
              package = package_name;
              error = Int.to_string result.failed_count ^ " packages failed to build"
            })

let build_package_in_workspace_root = fun ~registry ~workspace_root ~package_name ->
  match Workspace_manager.scan workspace_root with
  | Error error -> Error (WorkspaceScanFailed { workspace_root; error })
  | Ok (workspace, _load_errors) -> (
      match Workspace_resolution.ensure_workspace
        ~emit:(fun _ -> ())
        ~mode:Dep_solver.Refresh
        ~registry
        ~workspace
        () with
      | Error error -> Error (WorkspacePreparationFailed { error })
      | Ok workspace -> build_package_in_workspace ~workspace ~package_name
    )

let run_check = fun ~emit ~package_name ~stage check_fn ->
  emit (CheckStarted { package = package_name; stage });
  match check_fn () with
  | Error _ as err -> err
  | Ok () ->
      emit (CheckFinished { package = package_name; stage });
      Ok ()

let run_publish_checks = fun ~emit ~registry ~(original_workspace:Workspace.t) ~(resolved_workspace:Workspace.t) (
  package: Package.t
) ->
  match run_check
    ~emit
    ~package_name:package.name
    ~stage:`Fmt
    (fun () ->
      Tusk_fmt.run_check_paths
        ~workspace:original_workspace
        ~on_event:(fun event -> emit (Fmt event))
        [ package.path ]) with
  | Error exn -> Error (FmtCheckFailed { package = package.name; error = exn_message exn })
  | Ok () -> (
      match run_check
        ~emit
        ~package_name:package.name
        ~stage:`Fix
        (fun () ->
          Tusk_fix.Cli.run_check_paths
            ~cwd:original_workspace.root
            ~on_event:(fun event -> emit (Fix event))
            ~build_package:(fun ~workspace_root ~package_name ->
              build_package_in_workspace_root ~registry ~workspace_root ~package_name
              |> Result.map_error (fun err -> Failure (message err)))
            [ package.path ]) with
      | Error exn -> Error (FixCheckFailed { package = package.name; error = exn_message exn })
      | Ok () -> (
          match run_check
            ~emit
            ~package_name:package.name
            ~stage:`Build
            (fun () ->
              build_package_in_workspace ~workspace:resolved_workspace ~package_name:package.name) with
          | Error _ as err -> err
          | Ok () -> Ok ()
        )
    )

let rec run_packages = fun ~emit ~registry ~(original_workspace:Workspace.t) ~(resolved_workspace:Workspace.t) ~publishing_workspace_packages ~api_token_opt ~mode acc packages ->
  match packages with
  | [] -> Ok (List.rev acc)
  | package :: rest -> (
      match run_publish_checks ~emit ~registry ~original_workspace ~resolved_workspace package with
      | Error _ as err -> err
      | Ok () -> (
          match Publisher.prepare_publish
            ~registry
            ~target_dir_root:original_workspace.target_dir_root
            ~publishing_workspace_packages
            ~package with
          | Error err -> Error (PublishPlanFailed err)
          | Ok prepared -> (
              match mode, api_token_opt with
              | Dry_run, _ ->
                  emit (DryRunPlanned prepared);
                  run_packages
                    ~emit
                    ~registry
                    ~original_workspace
                    ~resolved_workspace
                    ~publishing_workspace_packages
                    ~api_token_opt
                    ~mode
                    (DryRun prepared :: acc)
                    rest
              | Publish, Some api_token -> (
                  match Publisher.publish_prepared ~registry ~api_token prepared with
                  | Error err -> Error (PublishFailed { package = package.name; error = err })
                  | Ok published ->
                      emit (PackagePublished published);
                      run_packages
                        ~emit
                        ~registry
                        ~original_workspace
                        ~resolved_workspace
                        ~publishing_workspace_packages
                        ~api_token_opt
                        ~mode
                        (Published published :: acc)
                        rest
                )
              | Publish, None ->
                  panic "expected API token in publish mode"
            )
        )
    )

let run = fun ?(on_event = no_event) ~(workspace:Workspace.t) ~request ~mode () ->
  match resolve_registry () with
  | Error _ as err -> err
  | Ok registry -> (
      match select_packages ~workspace request with
      | Error _ as err -> err
      | Ok packages -> (
          match Workspace_resolution.ensure_workspace
            ~emit:(fun kind -> on_event (Pm kind))
            ~mode:Dep_solver.Refresh
            ~registry
            ~workspace
            () with
          | Error error -> Error (WorkspacePreparationFailed { error })
          | Ok resolved_workspace -> (
              let publishing_workspace_packages =
                List.map (fun (pkg: Package.t) -> pkg.name) packages
              in
              let api_token_opt =
                match mode with
                | Dry_run -> Ok None
                | Publish -> load_api_token ~registry_name:(Pkgs_ml.Registry.name registry)
                |> Result.map (fun token -> Some token)
              in
              match api_token_opt with
              | Error _ as err -> err
              | Ok api_token_opt -> run_packages
                ~emit:on_event
                ~registry
                ~original_workspace:workspace
                ~resolved_workspace
                ~publishing_workspace_packages
                ~api_token_opt
                ~mode
                []
                packages
            )
        )
    )
