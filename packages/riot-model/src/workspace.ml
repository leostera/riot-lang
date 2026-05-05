open Std

type t = {
  name: string option;
  root: Path.t;
  target_dir_root: Path.t;
  source_ignore_patterns: string list;
  packages: Package_manifest.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
}

let resolve_target_dir_root = fun ~root ?target_dir () ->
  let target_dir =
    match target_dir with
    | Some target_dir -> target_dir
    | None -> "_build"
  in
  let target_dir_path = Path.v target_dir in
  if Path.is_absolute target_dir_path then
    target_dir_path
  else
    Path.(root / target_dir_path)

let make
  ?name
  ~root
  ~packages
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(profile_overrides = [])
  ?(source_ignore_patterns = [])
  ?target_dir
  () = {
  name;
  root;
  target_dir_root = resolve_target_dir_root ~root ?target_dir ();
  source_ignore_patterns;
  packages;
  dependencies;
  dev_dependencies;
  build_dependencies;
  profile_overrides;
}

let make_realized
  ?name
  ~root
  ~packages
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(profile_overrides = [])
  ?(source_ignore_patterns = [])
  ?target_dir
  () =
  make
    ?name
    ~root
    ~packages:(List.map packages ~fn:Package_manifest.from_package)
    ~dependencies
    ~dev_dependencies
    ~build_dependencies
    ~profile_overrides
    ~source_ignore_patterns
    ?target_dir
    ()

let dependencies_for_scope = fun scope (workspace: t) ->
  match scope with
  | Package.Normal -> workspace.dependencies
  | Package.Dev -> workspace.dependencies @ workspace.dev_dependencies
  | Package.Build -> workspace.build_dependencies

let package_root = fun (workspace: t) (pkg: Package_manifest.t) ->
  if Package_manifest.is_workspace_member pkg then
    Path.normalize Path.(workspace.root / pkg.relative_path)
  else
    Path.normalize pkg.path

let find_package_for_path = fun (workspace: t) ~path ->
  let path = Path.normalize path in
  let contains_path (pkg: Package_manifest.t) =
    let package_root = package_root workspace pkg in
    Path.equal path package_root || match Path.strip_prefix path ~prefix:package_root with
    | Ok _ -> true
    | Error _ -> false
  in
  workspace.packages
  |> List.filter ~fn:contains_path
  |> List.sort
    ~compare:(fun (left: Package_manifest.t) (right: Package_manifest.t) ->
      Int.compare
        (String.length (Path.to_string (package_root workspace right)))
        (String.length (Path.to_string (package_root workspace left))))
  |> fun __tmp1 ->
    match __tmp1 with
    | pkg :: _ -> Some pkg
    | [] -> None

let realize_package = fun ~intent (workspace: t) manifest ->
  Package_manifest.realize
    ~intent
    ~source_ignore_patterns:workspace.source_ignore_patterns
    manifest

let realize_packages = fun ~intent workspace ->
  List.map
    workspace.packages
    ~fn:(realize_package ~intent workspace)

let project_id = fun workspace ->
  let root_str = Path.to_string workspace.root in
  String.map
    root_str
    ~fn:(fun c ->
      if c = '/' then
        '-'
      else
        c)

let server_port = fun workspace ->
  let root_str = Path.to_string workspace.root in
  let hash = Std.Crypto.hash_string root_str in
  let hash_int = Std.Crypto.Digest.to_int hash in
  let port_range = 65_535 - 49_152 in
  50_152 + (Int.abs hash_int mod port_range)

let discover_commands: t -> Package_command.t list = fun workspace ->
  List.map workspace.packages ~fn:(fun (pkg: Package_manifest.t) -> pkg.commands)
  |> List.concat

let find_command: t -> string -> Package_command.t option = fun workspace name ->
  discover_commands workspace
  |> List.find ~fn:(fun (cmd: Package_command.t) -> cmd.name = name)

let discover_fix_providers: t -> Fix_provider.t list = fun workspace ->
  List.map workspace.packages ~fn:(fun (pkg: Package_manifest.t) -> pkg.fix_providers)
  |> List.concat
