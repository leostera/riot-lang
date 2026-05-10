open Std

type input = {
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  build_ctx: Riot_model.Build_ctx.t;
  toolchain: Riot_toolchain.t;
  depset: Riot_planner.Dependency.t list;
  sandbox_dir: Path.t;
  module_graph: Riot_planner.Module_node.t Graph.SimpleGraph.t;
  dep_analysis: Dep_analysis.t;
}

val cache_key_version: string

val plan: input -> (Action_execution.t list, Error.t) result
