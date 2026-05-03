open Std

(** Resolve the user-level Riot directory.

    Defaults to [$HOME/.riot]. When [RIOT_DIR] is set, that path is used
    instead so installed Riot binaries can keep their metadata, registry cache,
    toolchains, projects, and bin directory outside the default home location.
*)
val user_riot_dir: unit -> (Path.t, string) result

(** User-level Riot directory resolved once at process startup. *)
val dot_riot: Path.t

val config_path: unit -> Path.t

val workspace_riot_dir: workspace_root:Path.t -> Path.t

val workspace_operational_config_path: workspace_root:Path.t -> Path.t

val registry_dir: unit -> Path.t

val git_registry_host_dir: host:string -> Path.t

val git_registry_repo_dir: host:string -> owner:string -> repo:string -> Path.t

val package_lock_path: workspace_root:Path.t -> Path.t

val toolchains_dir: Toolchain_config.t -> Path.t

val project_dir: Workspace.t -> Path.t

val ensure_created: unit -> (unit, exn) result

(** Build directory configuration *)

(** Default name of the build directory when [riot.target_dir] is unset *)
val build_dir_name: string

(**
   Get the build directory root for a workspace. Respects [riot.target_dir] in
   the workspace [riot.toml], falling back to [_build].
*)
val build_dir_root: workspace_root:Path.t -> Path.t

(** Get current host target triple *)
val host_target: unit -> Target.t

(** Target-aware path functions - new API *)

(** Get profile directory (e.g., /path/to/project/_build/debug) *)
val profile_dir: workspace_root:Path.t -> profile:string -> Path.t

(** Get profile directory using an already-resolved workspace target root. *)
val profile_dir_in_workspace: workspace:Workspace.t -> profile:string -> Path.t

(** Get target directory within profile (e.g., /path/to/project/_build/debug/aarch64-apple-darwin) *)
val target_dir: workspace_root:Path.t -> profile:string -> target:Target.t -> Path.t

(** Get target directory within profile for a workspace. Respects [workspace.target_dir_root]. *)
val target_dir_in_workspace: workspace:Workspace.t -> profile:string -> target:Target.t -> Path.t

(** Get output directory for specific profile and target *)
val out_dir_with_target: workspace_root:Path.t -> profile:string -> target:Target.t -> Path.t

(** Get output directory for specific profile and target in a workspace. *)
val out_dir_in_workspace: workspace:Workspace.t -> profile:string -> target:Target.t -> Path.t

(** Get sandbox directory for specific profile and target *)
val sandbox_dir_with_target: workspace_root:Path.t -> profile:string -> target:Target.t -> Path.t

(** Get sandbox directory for specific profile and target in a workspace. *)
val sandbox_dir_in_workspace: workspace:Workspace.t -> profile:string -> target:Target.t -> Path.t

(** Get cache directory for specific profile and target *)
val cache_dir_with_target: workspace_root:Path.t -> profile:string -> target:Target.t -> Path.t

(** Get cache directory for specific profile and target in a workspace. *)
val cache_dir_in_workspace: workspace:Workspace.t -> profile:string -> target:Target.t -> Path.t

(** Get build lock path for a specific profile and target lane *)
val build_lock_path_with_target:
  workspace_root:Path.t ->
  profile:string ->
  target:Target.t ->
  Path.t

(** Get build lock path for a specific profile and target lane in a workspace. *)
val build_lock_path_in_workspace:
  workspace:Workspace.t ->
  profile:string ->
  target:Target.t ->
  Path.t

(** Backward compatible functions - default to debug profile + host target *)

(** Get the debug build directory (e.g., /path/to/project/_build/debug) *)
val debug_dir: workspace_root:Path.t -> Path.t

(** Get the cache directory (defaults to debug profile, host target) *)
val cache_dir: workspace_root:Path.t -> Path.t

(** Get the output directory (defaults to debug profile, host target) *)
val out_dir: workspace_root:Path.t -> Path.t

(** Get the sandbox directory (defaults to debug profile, host target) *)
val sandbox_dir: workspace_root:Path.t -> Path.t
