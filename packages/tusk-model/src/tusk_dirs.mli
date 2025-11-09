open Std

val dot_tusk : Path.t
val toolchains_dir : Toolchain_config.t -> Path.t
val project_dir : Workspace.t -> Path.t
val ensure_created : unit -> (unit, exn) result

(** Build directory configuration *)

(** Name of the build directory - centralized configuration point *)
val build_dir_name : string

(** Get the build directory root for a workspace (e.g., /path/to/project/target) *)
val build_dir_root : workspace_root:Path.t -> Path.t

(** Get the debug build directory (e.g., /path/to/project/target/debug) *)
val debug_dir : workspace_root:Path.t -> Path.t

(** Get the cache directory (e.g., /path/to/project/target/debug/cache) *)
val cache_dir : workspace_root:Path.t -> Path.t

(** Get the output directory (e.g., /path/to/project/target/debug/out) *)
val out_dir : workspace_root:Path.t -> Path.t

(** Get the sandbox directory (e.g., /path/to/project/target/debug/sandbox) *)
val sandbox_dir : workspace_root:Path.t -> Path.t
