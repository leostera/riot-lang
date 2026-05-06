open Std

type scope =
  | Runtime
  | Dev
type dev_artifacts = Riot_model.Package.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}
type t

val make:
  workspace:Riot_model.Workspace.t ->
  packages:Riot_model.Package_name.t list ->
  targets:Riot_model.Target.request ->
  scope:scope ->
  profile:Riot_model.Profile.t ->
  ?synthetic_tools:Riot_planner.Build_unit_graph.synthetic_tool list ->
  ?dev_artifacts:dev_artifacts ->
  ?requested_parallelism:int option ->
  unit ->
  t

module Internal: sig
  val workspace: t -> Riot_model.Workspace.t

  val packages: t -> Riot_model.Package_name.t list

  val targets: t -> Riot_model.Target.request

  val scope: t -> scope

  val dev_artifacts: t -> dev_artifacts

  val profile: t -> Riot_model.Profile.t

  val synthetic_tools: t -> Riot_planner.Build_unit_graph.synthetic_tool list

  val requested_parallelism: t -> int option
end
