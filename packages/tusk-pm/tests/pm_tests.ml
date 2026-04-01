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

let tests =
  Test.[
    case "dep solver: projects workspace packages into lockfile" test_lock_deps_projects_workspace_packages;
    case "dep solver: rejects path dependencies for now" test_lock_deps_rejects_path_dependencies_for_now;
  ]

let name = "Tusk PM Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
