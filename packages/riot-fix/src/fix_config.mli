open Std

type rule_state =
  | Enabled
  | Disabled
type rule_override = {
  name: string;
  state: rule_state;
}
type fix_config = {
  ignore_patterns: string list;
  rules: rule_override list;
}
type scope
val empty_fix_config: fix_config

(** Load workspace and package-local riot.fix configuration if [cwd] is inside a
    workspace. Returns [None] outside a workspace. *)
val load_scope: cwd:Path.t -> scope option

val workspace_root: scope -> Path.t

val target_dir_root: scope -> Path.t

val providers: scope option -> Riot_model.Fix_provider.t list

(** Effective ignore patterns to apply while scanning. Includes workspace-level
    patterns. *)
val ignore_patterns: scope option -> string list

(** Whether a file should be ignored by [riot fix] according to workspace and
    package-local config. *)
val should_ignore_file: scope option -> Path.t -> bool

(** Build the effective lint pipeline for a file by applying package-local rule
    overrides on top of the workspace rules. *)
val pipeline_for_file: scope option -> Path.t -> Pipeline.t
