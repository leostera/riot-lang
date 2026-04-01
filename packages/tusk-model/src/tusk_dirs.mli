open Std

val dot_tusk: Path.t

val package_lock_path: workspace_root:Path.t -> Path.t

val registry_dir: registry_name:string -> Path.t

val registry_index_dir: registry_name:string -> Path.t

val registry_archive_dir: registry_name:string -> Path.t

val registry_archive_path:
  registry_name:string ->
  package_name:string ->
  version:string ->
  Path.t

val registry_src_dir: registry_name:string -> Path.t

val registry_package_src_dir:
  registry_name:string ->
  package_name:string ->
  version:string ->
  Path.t

val toolchains_dir: Toolchain_config.t -> Path.t

val project_dir: Workspace.t -> Path.t

val ensure_created: unit -> (unit, exn) result
(** Build directory configuration *)

(** Default name of the build directory when [tusk.target_dir] is unset *)
val build_dir_name: string
(** Get the build directory root for a workspace. Respects [tusk.target_dir] in
    the workspace [tusk.toml], falling back to [_build]. *)
val build_dir_root: workspace_root:Path.t -> Path.t
(** Get current host triple *)
val host_target: unit -> string
(** Target-aware path functions - new API *)

(** Get profile directory (e.g., /path/to/project/_build/debug) *)
val profile_dir: workspace_root:Path.t -> profile:string -> Path.t
(** Get target directory within profile (e.g., /path/to/project/_build/debug/aarch64-apple-darwin) *)
val target_dir: workspace_root:Path.t -> profile:string -> target:string -> Path.t
(** Get output directory for specific profile and target *)
val out_dir_with_target: workspace_root:Path.t -> profile:string -> target:string -> Path.t
(** Get sandbox directory for specific profile and target *)
val sandbox_dir_with_target: workspace_root:Path.t -> profile:string -> target:string -> Path.t
(** Get cache directory for specific profile and target *)
val cache_dir_with_target: workspace_root:Path.t -> profile:string -> target:string -> Path.t
(** Get build lock path for a specific profile and target lane *)
val build_lock_path_with_target: workspace_root:Path.t -> profile:string -> target:string -> Path.t
(** Backward compatible functions - default to debug profile + host target *)

(** Get the debug build directory (e.g., /path/to/project/_build/debug) *)
val debug_dir: workspace_root:Path.t -> Path.t
(** Get the cache directory (defaults to debug profile, host target) *)
val cache_dir: workspace_root:Path.t -> Path.t
(** Get the output directory (defaults to debug profile, host target) *)
val out_dir: workspace_root:Path.t -> Path.t
(** Get the sandbox directory (defaults to debug profile, host target) *)
val sandbox_dir: workspace_root:Path.t -> Path.t
