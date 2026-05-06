open Std

type scope = Request.scope =
  | Runtime
  | Dev
type dev_artifacts = Request.dev_artifacts = { tests: bool; examples: bool; benches: bool }
type t
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

val package_names: t -> Riot_model.Package_name.t list

val targets: t -> Riot_model.Target.Set.t

val scope: t -> scope

val dev_artifacts: t -> dev_artifacts

val synthetic_tools: t -> Riot_planner.Build_unit_graph.synthetic_tool list

val resolve: Build_context.t -> Request.t -> (t, error) result
