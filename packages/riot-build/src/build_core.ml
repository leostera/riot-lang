open Std

type error =
  | TargetSelectionFailed of Riot_model.Target.resolve_error
  | PackageNotFound of {
      package_name: Riot_model.Package_name.t;
      available_packages: Riot_model.Package_name.t list
    }
  | PackagesNotFound of {
      package_names: Riot_model.Package_name.t list;
      available_packages: Riot_model.Package_name.t list
    }
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | BuildFailed of { errors: Riot_executor.Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | SessionStartFailed of { reason: string }
  | UnexpectedError of { reason: string }

let error_message = function
  | TargetSelectionFailed { pattern; available_targets } ->
      "No targets match pattern '" ^ pattern ^ "'. Available targets: "
      ^ (
          available_targets
          |> List.map ~fn:Riot_model.Target.to_string
          |> String.concat ", "
        )
  | PackageNotFound { package_name; available_packages } ->
      "Package '" ^ Riot_model.Package_name.to_string package_name ^ "' not found. Available packages: "
      ^ String.concat ", " (List.map available_packages ~fn:Riot_model.Package_name.to_string)
  | PackagesNotFound { package_names; available_packages } ->
      "Packages not found: "
      ^ String.concat ", " (List.map package_names ~fn:Riot_model.Package_name.to_string)
      ^ ". Available packages: "
      ^ String.concat ", " (List.map available_packages ~fn:Riot_model.Package_name.to_string)
  | ToolchainInstallFailed { target; error } ->
      "Failed to install toolchain for "
      ^ Riot_model.Target.to_string target
      ^ ": "
      ^ error
  | ToolchainInitializationFailed { target; error } ->
      "Failed to initialize toolchain for "
      ^ Riot_model.Target.to_string target
      ^ ": "
      ^ error
  | BuildFailed { errors } ->
      Client.error_message (Client.BuildFailed { errors })
  | PlanningFailed { reason } ->
      Client.error_message (Client.PlanningFailed { reason })
  | CycleDetected { cycle_nodes } ->
      Client.error_message (Client.CycleDetected { cycle_nodes })
  | BuildAlreadyRunning { lock_path } ->
      Client.error_message (Client.BuildAlreadyRunning { lock_path })
  | SessionStartFailed { reason }
  | UnexpectedError { reason } ->
      reason

let available_package_names = fun workspace ->
  Prepared_workspace.Internal.package_names workspace
  |> List.sort ~compare:Riot_model.Package_name.compare

let resolve_package_names = fun workspace requested ->
  let available = available_package_names workspace in
  match requested with
  | [] -> Ok available
  | [ package_name ] ->
      if
        List.any
          available
          ~fn:(fun available_package_name ->
            Riot_model.Package_name.equal available_package_name package_name)
      then
        Ok [ package_name ]
      else
        Error (PackageNotFound {
          package_name;
          available_packages = available
        })
  | package_names ->
      let missing =
        List.filter package_names ~fn:(fun package_name ->
            not
              (List.any
                 available
                 ~fn:(fun available_package_name ->
                   Riot_model.Package_name.equal available_package_name package_name)))
      in
      if List.is_empty missing then
        Ok package_names
      else
        Error (PackagesNotFound {
          package_names = missing;
          available_packages = available
        })

let resolve_target_names = fun workspace request ->
  let host = Riot_model.Target.current in
  let configured_targets =
    Riot_model.Target.configured_targets
      ~host
      (Riot_model.Toolchain_config.from_workspace
         (Prepared_workspace.Internal.workspace workspace))
  in
  Riot_model.Target.resolve
    ~host
    ~configured_targets
    (Request.Internal.targets request)
  |> Result.map_err ~fn:(fun err -> TargetSelectionFailed err)

let resolve = fun request ->
  let open Std.Result.Syntax in
  let workspace = Request.Internal.workspace request in
  let* package_names =
    resolve_package_names workspace (Request.Internal.packages request)
  in
  let* targets = resolve_target_names workspace request in
  Ok (Build_spec.make
    ~workspace
    ~package_names
    ~targets
    ~scope:(Request.Internal.scope request)
    ~profile:(Request.Internal.profile request))

let map_runtime_error = function
  | Build_runtime.ToolchainInstallFailed { target; error } ->
      ToolchainInstallFailed { target; error }
  | Build_runtime.ToolchainInitializationFailed { target; error } ->
      ToolchainInitializationFailed { target; error }
  | Build_runtime.ClientError (Client.PackageNotFound { package_name; available_packages }) ->
      PackageNotFound { package_name; available_packages }
  | Build_runtime.ClientError (Client.PackagesNotFound { package_names; available_packages }) ->
      PackagesNotFound { package_names; available_packages }
  | Build_runtime.ClientError (Client.BuildFailed { errors }) ->
      BuildFailed { errors }
  | Build_runtime.ClientError (Client.PlanningFailed { reason }) ->
      PlanningFailed { reason }
  | Build_runtime.ClientError (Client.CycleDetected { cycle_nodes }) ->
      CycleDetected { cycle_nodes }
  | Build_runtime.ClientError (Client.BuildAlreadyRunning { lock_path }) ->
      BuildAlreadyRunning { lock_path }
  | Build_runtime.ClientError (Client.StartupFailed { error }) ->
      SessionStartFailed { reason = Internal_server.error_message error }
  | Build_runtime.ClientError (Client.UnexpectedEvent { reason }) ->
      UnexpectedError { reason }

let execute_raw = fun ?(allow_partial_failures = false) ?(record_cache_generation = true) ?on_event spec ->
  let on_runtime_event =
    Option.map on_event ~fn:(fun emit ->
        fun (event: Build_runtime.build_event) ->
          match Event_bridge.of_build_runtime_event event with
          | Some event -> emit event
          | None -> ())
  in
  Build_runtime.execute
    ~allow_partial_failures
    ~record_cache_generation
    ?on_event:on_runtime_event
    spec
  |> Result.map_err ~fn:map_runtime_error

let execute = fun ?on_event spec ->
  execute_raw ?on_event spec
  |> Result.map ~fn:Output.of_build_results

let build = fun ?on_event request ->
  let open Std.Result.Syntax in
  let* spec = resolve request in
  execute ?on_event spec
