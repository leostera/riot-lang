open Std

type scope =
  | Runtime
  | Dev

type dev_artifacts = Riot_model.Package.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}

type t = {
  workspace: Riot_model.Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.request;
  scope: scope;
  dev_artifacts: dev_artifacts;
  profile: Riot_model.Profile.t;
  synthetic_tools: Riot_planner.Build_unit_graph.synthetic_tool list;
  requested_parallelism: int option;
}

let make = fun
  ~workspace
  ~packages
  ~targets
  ~scope
  ~profile
  ?(synthetic_tools = [])
  ?(dev_artifacts = {tests = true; examples = true; benches = true})
  ?(requested_parallelism = None)
  () ->
  {
    workspace;
    packages;
    targets;
    scope;
    dev_artifacts;
    profile;
    synthetic_tools;
    requested_parallelism;
  }

module Internal = struct
  let workspace = fun t -> t.workspace

  let packages = fun t -> t.packages

  let targets = fun t -> t.targets

  let scope = fun t -> t.scope

  let dev_artifacts = fun t -> t.dev_artifacts

  let profile = fun t -> t.profile

  let synthetic_tools = fun t -> t.synthetic_tools

  let requested_parallelism = fun t -> t.requested_parallelism
end
