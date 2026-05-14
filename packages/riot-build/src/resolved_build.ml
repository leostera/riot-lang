open Std

type scope = Request.scope =
  | Runtime
  | Dev
  | Dependencies

type dev_artifacts = Request.dev_artifacts = { tests: bool; examples: bool; benches: bool }

type t = {
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: scope;
  dev_artifacts: dev_artifacts;
  synthetic_tools: Riot_planner.Build_unit_graph.synthetic_tool list;
  include_external_packages: bool;
}

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

let make = fun
  ~package_names ~targets ~scope ~dev_artifacts ~synthetic_tools ~include_external_packages ->
  {
    package_names;
    targets;
    scope;
    dev_artifacts;
    synthetic_tools;
    include_external_packages;
  }

let package_names = fun t -> t.package_names

let targets = fun t -> t.targets

let scope = fun t -> t.scope

let dev_artifacts = fun t -> t.dev_artifacts

let synthetic_tools = fun t -> t.synthetic_tools

let include_external_packages = fun t -> t.include_external_packages

let available_package_names = fun ~include_external_packages workspace ->
  workspace.Riot_model.Workspace.packages
  |> List.filter
    ~fn:(fun package ->
      include_external_packages || Riot_model.Package_manifest.is_workspace_member package)
  |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name)
  |> List.sort ~compare:Riot_model.Package_name.compare

let resolve_package_names = fun ~include_external_packages workspace requested ->
  let available = available_package_names ~include_external_packages workspace in
  match requested with
  | [] -> Ok available
  | [ package_name ] ->
      if
        List.any
          available
          ~fn:(fun available_package_name ->
            Riot_model.Package_name.equal
              available_package_name
              package_name)
      then
        Ok [ package_name ]
      else
        Error (PackageNotFound { package_name; available_packages = available })
  | package_names ->
      let missing =
        List.filter
          package_names
          ~fn:(fun package_name ->
            not
              (List.any
                available
                ~fn:(fun available_package_name ->
                  Riot_model.Package_name.equal
                    available_package_name
                    package_name)))
      in
      if List.is_empty missing then
        Ok package_names
      else
        Error (PackagesNotFound { package_names = missing; available_packages = available })

let resolve_target_names = fun context request ->
  let host = context.Build_context.host in
  let configured_targets =
    Riot_model.Target.configured_targets ~host context.Build_context.toolchain_config
  in
  Riot_model.Target.resolve ~host ~configured_targets (Request.Internal.targets request)
  |> Result.map_err ~fn:(fun err -> TargetSelectionFailed err)

let resolve = fun context request ->
  let open Std.Result.Syntax in
  let include_external_packages = Request.Internal.include_external_packages request in
  let* package_names =
    resolve_package_names
      ~include_external_packages
      context.Build_context.workspace
      (Request.Internal.packages request)
  in
  let* targets = resolve_target_names context request in
  Ok (make
    ~package_names
    ~targets
    ~scope:(Request.Internal.scope request)
    ~dev_artifacts:(Request.Internal.dev_artifacts request)
    ~synthetic_tools:(Request.Internal.synthetic_tools request)
    ~include_external_packages)
