open Std

type resolve_error =
  | TargetSelectionFailed of Riot_model.Target.resolve_error
  | PackageNotFound of { package_name: string; available_packages: string list }
  | PackagesNotFound of { package_names: string list; available_packages: string list }

type build_error = Build_runtime.build_error =
  | NoTargetsMatched of Riot_model.Target.resolve_error
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | ClientError of Client.error

let resolve_error_message = function
  | TargetSelectionFailed { pattern; available_targets } ->
      "No targets match pattern '" ^ pattern ^ "'. Available targets: "
      ^ (
          available_targets
          |> List.map ~fn:Riot_model.Target.to_string
          |> String.concat ", "
        )
  | PackageNotFound { package_name; available_packages } ->
      "Package '" ^ package_name ^ "' not found. Available packages: "
      ^ String.concat ", " available_packages
  | PackagesNotFound { package_names; available_packages } ->
      "Packages not found: " ^ String.concat ", " package_names
      ^ ". Available packages: " ^ String.concat ", " available_packages

let build_error_message = Build_runtime.error_message

let available_package_names = fun workspace ->
  Prepared_workspace.package_names workspace |> List.sort ~compare:String.compare

let resolve_package_names = fun workspace requested ->
  let available = available_package_names workspace in
  match requested with
  | [] -> Ok available
  | [ package_name ] ->
      if List.contains available ~value:package_name then
        Ok [ package_name ]
      else
        Error (PackageNotFound { package_name; available_packages = available })
  | package_names ->
      let missing =
        List.filter package_names ~fn:(fun package_name ->
            not (List.contains available ~value:package_name))
      in
      if List.is_empty missing then
        Ok package_names
      else
        Error (PackagesNotFound { package_names = missing; available_packages = available })

let resolve_target_names = fun workspace request ->
  let host = Riot_model.Target.current in
  let configured_targets =
    Riot_model.Target.configured_targets
      ~host
      (Riot_model.Toolchain_config.from_workspace (Prepared_workspace.workspace workspace))
  in
  Riot_model.Target.resolve
    ~host
    ~configured_targets
    (Request.targets request)
  |> Result.map_err ~fn:(fun err -> TargetSelectionFailed err)

let resolve = fun workspace request ->
  let open Std.Result.Syntax in
  let* package_names = resolve_package_names workspace (Request.packages request) in
  let* targets = resolve_target_names workspace request in
  Ok (Build_spec.make
    ~workspace
    ~package_names
    ~targets
    ~scope:(Request.scope request)
    ~profile:(Request.profile request))

let build = fun ?on_event spec ->
  let workspace = Prepared_workspace.workspace (Build_spec.workspace spec) in
  let workspace_manager =
    Prepared_workspace.workspace_manager (Build_spec.workspace spec)
  in
  Build_runtime.build
    ?on_event
    ?workspace_manager
    {
      workspace;
      packages = Build_spec.package_names spec;
      targets = (
        if Riot_model.Target.Set.is_empty (Build_spec.targets spec) then
          Riot_model.Target.Host
        else
          Riot_model.Target.Exact (Build_spec.targets spec)
      );
      scope = Build_spec.scope spec;
      profile = (Build_spec.profile spec).name;
    }
  |> Result.map ~fn:Output.of_build_results
