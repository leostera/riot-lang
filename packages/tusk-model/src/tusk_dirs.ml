open Std

let dot_tusk =
  let home =
    match Env.home_dir () with
    | Some h -> h
    | None -> panic "Failed to get home directory"
  in
  Path.(home / Path.v ".tusk")

let package_lock_path = fun ~workspace_root -> Path.(workspace_root / Path.v "tusk.lock")

let registry_dir = fun ~registry_name -> Path.(dot_tusk / Path.v "registry" / Path.v registry_name)

let registry_index_dir = fun ~registry_name ->
  Path.(registry_dir ~registry_name / Path.v "index")

let registry_archive_dir = fun ~registry_name ->
  Path.(registry_dir ~registry_name / Path.v "archive")

let registry_archive_path = fun ~registry_name ~package_name ~version ->
  Path.(
    registry_archive_dir ~registry_name
    / Path.v package_name
    / Path.v (version ^ ".tar"))

let registry_src_dir = fun ~registry_name ->
  Path.(registry_dir ~registry_name / Path.v "src")

let registry_package_src_dir = fun ~registry_name ~package_name ~version ->
  Path.(
    registry_src_dir ~registry_name
    / Path.v package_name
    / Path.v version)

let toolchains_dir = fun toolchain_config ->
  let version = toolchain_config.Toolchain_config.version in
  Path.(dot_tusk / Path.v "toolchains" / Path.v version)

let project_dir = fun workspace ->
  let project_id = Workspace.project_id workspace in
  Path.(dot_tusk / Path.v "projects" / Path.v project_id)

let ensure_created = fun () ->
  let _ = Fs.create_dir_all dot_tusk in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "projects") in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "toolchains") in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "bin") in
  Ok ()
(** Build directory configuration - single source of truth *)
let build_dir_name = "_build"

(* Note: The following functions don't reference Workspace type to avoid circular dependency.
   They use the workspace root path directly. *)

let resolve_build_dir_root = fun ~workspace_root target_dir ->
  let target_dir_path = Path.v target_dir in
  if Path.is_absolute target_dir_path then
    target_dir_path
  else
    Path.(workspace_root / target_dir_path)

let workspace_build_dir_name = fun ~workspace_root ->
  let toml_path = Path.(workspace_root / Path.v "tusk.toml") in
  match Fs.read_to_string toml_path with
  | Error _ -> build_dir_name
  | Ok content -> (
      match Data.Toml.parse content with
      | Error _ -> build_dir_name
      | Ok toml -> (
          match Workspace.of_toml toml with
          | Ok manifest -> (
              match manifest.target_dir with
              | Some target_dir -> target_dir
              | None -> build_dir_name
            )
          | Error _ -> build_dir_name
        )
    )

let build_dir_root = fun ~workspace_root ->
  resolve_build_dir_root ~workspace_root (workspace_build_dir_name ~workspace_root)
(** Get current host triple *)
let host_target = fun () -> System.Host.to_string System.host_triplet
(** New target-aware path functions *)
let profile_dir = fun ~workspace_root ~profile ->
  Path.(build_dir_root ~workspace_root / Path.v profile)

let target_dir = fun ~workspace_root ~profile ~target ->
  Path.(profile_dir ~workspace_root ~profile / Path.v target)

let out_dir_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "out")

let sandbox_dir_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "sandbox")

let cache_dir_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "cache")

let build_lock_path_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "tusk.lock")
(** Backward compatible functions - default to debug profile + host target *)
let debug_dir = fun ~workspace_root -> profile_dir ~workspace_root ~profile:"debug"

let cache_dir = fun ~workspace_root ->
  cache_dir_with_target ~workspace_root ~profile:"debug" ~target:(host_target ())

let out_dir = fun ~workspace_root ->
  out_dir_with_target ~workspace_root ~profile:"debug" ~target:(host_target ())

let sandbox_dir = fun ~workspace_root ->
  sandbox_dir_with_target ~workspace_root ~profile:"debug" ~target:(host_target ())

module Tests = struct
  let test_package_lock_path () : (unit, string) result =
    let actual =
      package_lock_path ~workspace_root:(Path.v "/tmp/workspace")
      |> Path.to_string
    in
    if String.equal actual "/tmp/workspace/tusk.lock" then
      Ok ()
    else
      Error ("expected root tusk.lock path, got " ^ actual)
    [@test]

  let test_registry_split_layout () : (unit, string) result =
    let index = registry_index_dir ~registry_name:"pkgs.ml" |> Path.to_string in
    let archive =
      registry_archive_path
        ~registry_name:"pkgs.ml"
        ~package_name:"std"
        ~version:"0.1.0"
      |> Path.to_string
    in
    let src =
      registry_package_src_dir
        ~registry_name:"pkgs.ml"
        ~package_name:"std"
        ~version:"0.1.0"
      |> Path.to_string
    in
    let home =
      match Env.home_dir () with
      | Some path -> path
      | None -> panic "expected home directory for tests"
    in
    let prefix = Path.(home / Path.v ".tusk" / Path.v "registry" / Path.v "pkgs.ml") |> Path.to_string in
    if
      String.equal index (prefix ^ "/index")
      && String.equal archive (prefix ^ "/archive/std/0.1.0.tar")
      && String.equal src (prefix ^ "/src/std/0.1.0")
    then
      Ok ()
    else
      Error
        ("unexpected registry layout:\nindex="
        ^ index
        ^ "\narchive="
        ^ archive
        ^ "\nsrc="
        ^ src)
    [@test]
end [@test]
