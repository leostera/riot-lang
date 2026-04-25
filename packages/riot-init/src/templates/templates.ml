open Std.Result.Syntax

type config = Context.t = {
  on_event: Types.event -> unit;
  target_dir: Std.Path.t;
  workspace_name: string;
  package_name: string;
  is_library: bool;
}

type error = Context.error

let materialize = fun config ->
  let* () = Workspace_toml.materialize config
  in
  let* () = Toolchain_toml.materialize config
  in
  let* () = Gitignore.materialize config
  in
  let* () = Embedded_skill.materialize config
  in
  let* () = Dev_config.materialize config
  in
  let* () = Pre_commit_hook.materialize config
  in
  let* () = Riot_config.materialize config
  in
  let* () = Readme.materialize config
  in
  let* () = Dockerfile.materialize config
  in
  let* () = Ci_workflow.materialize config in Default_package.materialize config
