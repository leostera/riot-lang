open Std

let dot_tusk =
  let home =
    match Env.home_dir () with
    | Some h -> h
    | None -> panic "Failed to get home directory"
  in
  Path.(home / Path.v ".tusk")

let toolchains_dir toolchain_config =
  let version = toolchain_config.Toolchain_config.version in
  Path.(dot_tusk / Path.v "toolchains" / Path.v version)

let project_dir workspace =
  let project_id = Workspace.project_id workspace in
  Path.(dot_tusk / Path.v "projects" / Path.v project_id)

let ensure_created () =
  let _ = Fs.create_dir_all dot_tusk in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "projects") in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "toolchains") in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "bin") in
  Ok ()

(** Build directory configuration - single source of truth *)
let build_dir_name = "_build"

(* Note: The following functions don't reference Workspace type to avoid circular dependency.
   They use the workspace root path directly. *)

let build_dir_root ~workspace_root =
  Path.(workspace_root / Path.v build_dir_name)

let debug_dir ~workspace_root =
  Path.(build_dir_root ~workspace_root / Path.v "debug")

let cache_dir ~workspace_root =
  Path.(debug_dir ~workspace_root / Path.v "cache")

let out_dir ~workspace_root =
  Path.(debug_dir ~workspace_root / Path.v "out")

let sandbox_dir ~workspace_root =
  Path.(debug_dir ~workspace_root / Path.v "sandbox")
