open Std

type toolchain_install_error = Build_runtime.toolchain_install_error =
  | ToolchainDownloadFailed of { message: string }

let toolchain_install_error_message = Build_runtime.toolchain_install_error_message

type toolchain_initialization_error = Build_runtime.toolchain_initialization_error =
  | ToolchainInitFailed of { message: string }

let toolchain_initialization_error_message = Build_runtime.toolchain_initialization_error_message

type error =
  | TargetSelectionFailed of Riot_model.Target.resolve_error
  | PackageNotFound of {
      package_name: Riot_model.Package_name.t;
      available_packages: Riot_model.Package_name.t list;
    }
  | PackagesNotFound of {
      package_names: Riot_model.Package_name.t list;
      available_packages: Riot_model.Package_name.t list;
    }
  | ToolchainInstallFailed of {
      target: Riot_model.Target.t;
      error: toolchain_install_error;
    }
  | ToolchainInitializationFailed of {
      target: Riot_model.Target.t;
      error: toolchain_initialization_error;
    }
  | BuildFailed of {
      errors: Build_result.failure list;
    }
  | PlanningFailed of Riot_planner.Workspace_planner.plan_error
  | CycleDetected of {
      cycle_nodes: string list;
    }
  | BuildAlreadyRunning of {
      lock_path: Path.t;
    }
  | InvalidRequestedParallelism of int
  | UnexpectedError of { reason: string }

let error_message = fun __tmp1 ->
  match __tmp1 with
  | TargetSelectionFailed { pattern; available_targets } ->
      "No targets match pattern '"
      ^ pattern
      ^ "'. Available targets: "
      ^ (
        available_targets
        |> List.map ~fn:Riot_model.Target.to_string
        |> String.concat ", "
      )
  | PackageNotFound { package_name; available_packages } ->
      "Package '"
      ^ Riot_model.Package_name.to_string package_name
      ^ "' not found. Available packages: "
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
      ^ toolchain_install_error_message error
  | ToolchainInitializationFailed { target; error } ->
      "Failed to initialize toolchain for "
      ^ Riot_model.Target.to_string target
      ^ ": "
      ^ toolchain_initialization_error_message error
  | BuildFailed { errors } -> (
      match errors with
      | [] -> "build failed"
      | [ failure ] -> Build_result.failure_message failure
      | failures ->
          "build failed:\n"
          ^ String.concat "\n" (List.map failures ~fn:Build_result.failure_message)
    )
  | PlanningFailed error ->
      "planning failed: " ^ Build_lane.error_message (Build_lane.PlanningFailed error)
  | CycleDetected { cycle_nodes } ->
      "cyclic dependency detected: " ^ String.concat " -> " cycle_nodes
  | BuildAlreadyRunning { lock_path } ->
      "another riot build is already running (" ^ Path.to_string lock_path ^ ")"
  | InvalidRequestedParallelism value ->
      "invalid requested parallelism (" ^ Int.to_string value ^ "): jobs must be >= 1"
  | UnexpectedError { reason } -> reason

let map_context_error = fun (Build_context.InvalidRequestedParallelism requested) ->
  InvalidRequestedParallelism requested

let map_resolved_error = fun __tmp1 ->
  match __tmp1 with
  | Resolved_build.TargetSelectionFailed error -> TargetSelectionFailed error
  | Resolved_build.PackageNotFound { package_name; available_packages } ->
      PackageNotFound { package_name; available_packages }
  | Resolved_build.PackagesNotFound { package_names; available_packages } ->
      PackagesNotFound { package_names; available_packages }

let make_context = fun ?on_event request ->
  Build_context.make ?on_event request
  |> Result.map_err ~fn:map_context_error

let resolve = fun context request ->
  Resolved_build.resolve context request
  |> Result.map_err ~fn:map_resolved_error

let map_runtime_error = fun __tmp1 ->
  match __tmp1 with
  | Build_runtime.ToolchainInstallFailed { target; error } ->
      ToolchainInstallFailed { target; error }
  | Build_runtime.ToolchainInitializationFailed { target; error } ->
      ToolchainInitializationFailed { target; error }
  | Build_runtime.BuildFailed { errors } ->
      BuildFailed { errors = Build_result.failures_of_build_results errors }
  | Build_runtime.PlanningFailed error -> PlanningFailed error
  | Build_runtime.UnexpectedError { reason } -> UnexpectedError { reason }

let execute_raw = fun
  ?(allow_partial_failures = false) ?(record_cache_generation = true) context spec ->
  Build_runtime.execute ~allow_partial_failures ~record_cache_generation context spec
  |> Result.map_err ~fn:map_runtime_error

let execute = fun context spec ->
  execute_raw context spec
  |> Result.map ~fn:Build_result.from_build_results

let build = fun ?on_event request ->
  let open Std.Result.Syntax in
  let* context = make_context ?on_event request in
  let* spec = resolve context request in
  execute context spec
