open Std

let user_riot_dir = fun () ->
  match Env.get Env.String ~var:"RIOT_DIR" with
  | Some path when not (String.equal path "") -> Ok (Path.v path)
  | Some _
  | None -> (
      match Env.home_dir () with
      | Some home -> Ok Path.(home / Path.v ".riot")
      | None -> Error "failed to determine home directory"
    )

let dot_riot =
  match user_riot_dir () with
  | Ok path -> path
  | Error message -> panic message

let config_path = fun () -> Path.(dot_riot / Path.v "config.toml")

let workspace_riot_dir = fun ~workspace_root -> Path.(workspace_root / Path.v ".riot")

let workspace_operational_config_path = fun ~workspace_root ->
  Path.(workspace_riot_dir ~workspace_root / Path.v "config.toml")

let registry_dir = fun () ->
  match Env.get Env.String ~var:"RIOT_REGISTRY_DIR" with
  | Some path -> Path.v path
  | None -> Path.(dot_riot / Path.v "registry")

let git_registry_host_dir = fun ~host -> Path.(registry_dir () / Path.v host)

let git_registry_repo_dir = fun ~host ~owner ~repo ->
  Path.(git_registry_host_dir ~host / Path.v owner / Path.v repo)

let package_lock_path = fun ~workspace_root -> Path.(workspace_root / Path.v "riot.lock")

let toolchains_dir = fun toolchain_config ->
  let version = toolchain_config.Toolchain_config.version in
  Path.(dot_riot / Path.v "toolchains" / Path.v version)

let project_dir = fun workspace ->
  let project_id = Workspace.project_id workspace in
  Path.(dot_riot / Path.v "projects" / Path.v project_id)

let ensure_created = fun () ->
  let _ = Fs.create_dir_all dot_riot in
  let _ = Fs.create_dir_all (registry_dir ()) in
  let _ = Fs.create_dir_all Path.(dot_riot / Path.v "projects") in
  let _ = Fs.create_dir_all Path.(dot_riot / Path.v "toolchains") in
  let _ = Fs.create_dir_all Path.(dot_riot / Path.v "bin") in
  let config = config_path () in
  let _ =
    match Fs.exists config with
    | Ok true -> Ok ()
    | Ok false -> User_config.save User_config.default config
    | Error _ -> Ok ()
  in
  Ok ()

(** Build directory configuration - single source of truth *)
let build_dir_name = "_build"

(* Note: The following functions don't reference Workspace type to avoid circular dependency.
   They use the workspace root path directly.
*)

let resolve_build_dir_root = fun ~workspace_root target_dir ->
  let target_dir_path = Path.v target_dir in
  if Path.is_absolute target_dir_path then
    target_dir_path
  else
    Path.(workspace_root / target_dir_path)

let workspace_build_dir_name = fun ~workspace_root ->
  let toml_path = Path.(workspace_root / Path.v "riot.toml") in
  match Fs.read_to_string toml_path with
  | Error _ -> build_dir_name
  | Ok content -> (
      match Data.Toml.parse content with
      | Error _ -> build_dir_name
      | Ok toml -> (
          match Workspace_manifest.from_toml toml with
          | Ok manifest -> (
              match manifest.target_dir with
              | Some target_dir -> target_dir
              | None -> build_dir_name
            )
          | Error _ -> build_dir_name
        )
    )

let build_dir_root = fun ~workspace_root ->
  resolve_build_dir_root
    ~workspace_root
    (workspace_build_dir_name ~workspace_root)

(** Get current host triple *)
let host_target = fun () -> Target.current

let target_dir_name = fun target -> Target.to_string target

(** New target-aware path functions *)
let profile_dir = fun ~workspace_root ~profile ->
  Path.(build_dir_root ~workspace_root / Path.v profile)

let profile_dir_in_workspace = fun ~(workspace:Workspace.t) ~profile ->
  Path.(workspace.target_dir_root / Path.v profile)

let target_dir = fun ~workspace_root ~profile ~target ->
  Path.(profile_dir ~workspace_root ~profile / Path.v (target_dir_name target))

let target_dir_in_workspace = fun ~(workspace:Workspace.t) ~profile ~target ->
  Path.(profile_dir_in_workspace ~workspace ~profile / Path.v (target_dir_name target))

let out_dir_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "out")

let out_dir_in_workspace = fun ~(workspace:Workspace.t) ~profile ~target ->
  Path.(target_dir_in_workspace ~workspace ~profile ~target / Path.v "out")

let sandbox_dir_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "sandbox")

let sandbox_dir_in_workspace = fun ~(workspace:Workspace.t) ~profile ~target ->
  Path.(target_dir_in_workspace ~workspace ~profile ~target / Path.v "sandbox")

let cache_dir_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "cache")

let cache_dir_in_workspace = fun ~(workspace:Workspace.t) ~profile ~target ->
  Path.(target_dir_in_workspace ~workspace ~profile ~target / Path.v "cache")

let build_lock_path_with_target = fun ~workspace_root ~profile ~target ->
  Path.(target_dir ~workspace_root ~profile ~target / Path.v "riot.lock")

let build_lock_path_in_workspace = fun ~(workspace:Workspace.t) ~profile ~target ->
  Path.(target_dir_in_workspace ~workspace ~profile ~target / Path.v "riot.lock")

(** Backward compatible functions - default to debug profile + host target *)
let debug_dir = fun ~workspace_root -> profile_dir ~workspace_root ~profile:"debug"

let cache_dir = fun ~workspace_root ->
  cache_dir_with_target
    ~workspace_root
    ~profile:"debug"
    ~target:(host_target ())

let out_dir = fun ~workspace_root ->
  out_dir_with_target
    ~workspace_root
    ~profile:"debug"
    ~target:(host_target ())

let sandbox_dir = fun ~workspace_root ->
  sandbox_dir_with_target
    ~workspace_root
    ~profile:"debug"
    ~target:(host_target ())

module Tests = struct
  let test_package_lock_path () =
    let actual =
      package_lock_path ~workspace_root:(Path.v "/tmp/workspace")
      |> Path.to_string
    in
    if String.equal actual "/tmp/workspace/riot.lock" then
      Ok ()
    else
      Error ("expected root riot.lock path, got " ^ actual) [@test]

  let test_workspace_target_dirs_use_custom_target_dir_root () =
    let workspace =
      Workspace.make ~root:(Path.v "/tmp/workspace") ~target_dir:"build-out" ~packages:[] ()
    in
    let target = host_target () in
    let expected_target_dir = "/tmp/workspace/build-out/release/" ^ Target.to_string target in
    let target_dir =
      target_dir_in_workspace ~workspace ~profile:"release" ~target
      |> Path.to_string
    in
    let out_dir =
      out_dir_in_workspace ~workspace ~profile:"release" ~target
      |> Path.to_string
    in
    let sandbox_dir =
      sandbox_dir_in_workspace ~workspace ~profile:"release" ~target
      |> Path.to_string
    in
    let cache_dir =
      cache_dir_in_workspace ~workspace ~profile:"release" ~target
      |> Path.to_string
    in
    let lock_path =
      build_lock_path_in_workspace ~workspace ~profile:"release" ~target
      |> Path.to_string
    in
    if not (String.equal target_dir expected_target_dir) then
      Error ("expected custom target dir root, got " ^ target_dir)
    else if not (String.equal out_dir (expected_target_dir ^ "/out")) then
      Error ("expected out dir inside custom target dir, got " ^ out_dir)
    else if not (String.equal sandbox_dir (expected_target_dir ^ "/sandbox")) then
      Error ("expected sandbox dir inside custom target dir, got " ^ sandbox_dir)
    else if not (String.equal cache_dir (expected_target_dir ^ "/cache")) then
      Error ("expected cache dir inside custom target dir, got " ^ cache_dir)
    else if not (String.equal lock_path (expected_target_dir ^ "/riot.lock")) then
      Error ("expected build lock path inside custom target dir, got " ^ lock_path)
    else
      Ok () [@test]
end [@test]
