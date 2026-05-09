open Std

type t = {
  build: Goal.build_package;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  toolchain: Riot_toolchain.t;
  build_ctx: Riot_model.Build_ctx.t;
  action_executions: Action_execution.t list;
  sandbox_dir: Path.t;
  package_hash: Crypto.hash;
}
