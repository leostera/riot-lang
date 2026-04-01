open Std
module Test = Std.Test

let make_sources = fun () ->
  Tusk_model.Package.{
    src = [];
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let make_package = fun ?(dependencies = []) ?(build_dependencies = []) ?(dev_dependencies = []) ~name ~path () ->
  Tusk_model.Package.{
    name;
    path;
    relative_path = path;
    dependencies;
    dev_dependencies;
    build_dependencies;
    foreign_dependencies = [];
    binaries = [];
    library = None;
    sources = make_sources ();
    compiler = { profile_overrides = []; target_overrides = [] };
    commands = [];
    fix_providers = [];
  }

let test_lock_deps_projects_workspace_packages = fun () ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg =
    make_package
      ~name:"app"
      ~path:(Path.v "/workspace/packages/app")
      ~dependencies:[ { name = "std"; source = Tusk_model.Package.Workspace } ]
      ~build_dependencies:[ { name = "std"; source = Tusk_model.Package.Workspace } ]
      ()
  in
  match Tusk_pm.Dep_solver.lock_deps ~mode:Refresh ~registry_name:"pkgs.ml" [ app_pkg; std_pkg ] with
  | Error err -> Error ("expected workspace lock projection to succeed: " ^ err)
  | Ok lockfile ->
      let app_lock = List.hd lockfile.packages in
      let std_lock = List.nth lockfile.packages 1 in
      let app_manifest = Path.to_string app_lock.manifest_path in
      if
        lockfile.format_version = 1
        && app_lock.id.name = "app"
        && std_lock.id.name = "std"
        && app_lock.provenance = Tusk_model.Lockfile.Workspace
        && std_lock.provenance = Tusk_model.Lockfile.Workspace
        && List.length app_lock.dependencies = 1
        && List.length app_lock.build_dependencies = 1
        && (List.hd app_lock.dependencies).package.name = "std"
        && String.equal app_manifest "/workspace/packages/app/tusk.toml"
      then
        Ok ()
      else
        Error "expected workspace packages to be projected into the lockfile"

let test_lock_deps_rejects_path_dependencies_for_now = fun () ->
  let vendor_pkg =
    make_package
      ~name:"vendor-consumer"
      ~path:(Path.v "/workspace/packages/vendor-consumer")
      ~dependencies:[ { name = "foo"; source = Tusk_model.Package.Path (Path.v "../vendor/foo") } ]
      ()
  in
  match Tusk_pm.Dep_solver.lock_deps ~mode:Refresh ~registry_name:"pkgs.ml" [ vendor_pkg ] with
  | Ok _ -> Error "expected path dependencies to fail until materialization exists"
  | Error err ->
      if String.contains err "path dependencies are not implemented in tusk-pm yet" then
        Ok ()
      else
        Error ("unexpected error: " ^ err)

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_lock_refresh_requires_lock_when_missing = fun () ->
  with_tempdir "tusk_pm_missing_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected manifest write to succeed";
      match Tusk_pm.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok true -> Ok ()
      | Ok false -> Error "expected missing lockfile to require refresh"
      | Error err -> Error err)

let test_lock_refresh_false_when_lock_is_newer = fun () ->
  with_tempdir "tusk_pm_fresh_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected manifest write to succeed";
      sleep (Time.Duration.from_millis 20);
      Fs.write "format_version = 1\npackages = []\n" lock_path
      |> Result.expect ~msg:"expected lockfile write to succeed";
      match Tusk_pm.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok false -> Ok ()
      | Ok true -> Error "expected newer lockfile to avoid refresh"
      | Error err -> Error err)

let test_lock_refresh_true_when_manifest_is_newer = fun () ->
  with_tempdir "tusk_pm_stale_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "[workspace]\nmembers = []\n" manifest_path
      |> Result.expect ~msg:"expected manifest write to succeed";
      Fs.write "format_version = 1\npackages = []\n" lock_path
      |> Result.expect ~msg:"expected lockfile write to succeed";
      sleep (Time.Duration.from_millis 20);
      Fs.write "[workspace]\nmembers = [\"packages/demo\"]\n" manifest_path
      |> Result.expect ~msg:"expected manifest rewrite to succeed";
      match Tusk_pm.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok true -> Ok ()
      | Ok false -> Error "expected newer manifest to require refresh"
      | Error err -> Error err)

let tests =
  Test.[
    case "dep solver: projects workspace packages into lockfile" test_lock_deps_projects_workspace_packages;
    case "dep solver: rejects path dependencies for now" test_lock_deps_rejects_path_dependencies_for_now;
    case "lock refresh: missing lock requires refresh" test_lock_refresh_requires_lock_when_missing;
    case "lock refresh: newer lock avoids refresh" test_lock_refresh_false_when_lock_is_newer;
    case "lock refresh: newer manifest requires refresh" test_lock_refresh_true_when_manifest_is_newer;
  ]

let name = "Tusk PM Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
