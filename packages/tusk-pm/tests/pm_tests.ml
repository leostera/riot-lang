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

let make_registry_cache = fun () ->
  Pkgs_ml.Registry_cache.create ~tusk_home:(Path.v "/Users/example/.tusk") ~registry_name:"pkgs.ml"
  |> Result.expect ~msg:"expected registry cache to initialize"

let make_release = fun ?(dependencies = []) ~version () ->
  Pkgs_ml.Sparse_index.{
    version;
    published_at = "2026-04-01T00:00:00Z";
    canonical_locator = "github.com/example/" ^ version;
    repo_url = "https://github.com/example/repo";
    subdir = ".";
    sha = "deadbeef";
    description = None;
    license = Some "Apache-2.0";
    homepage = None;
    repository = Some "https://github.com/example/repo";
    root_module = None;
    categories = [];
    keywords = [];
    manifest_key = "manifests/" ^ version ^ ".json";
    source_key = "sources/" ^ version ^ ".tar.gz";
    dependencies;
  }

let make_registry_dependency = fun name ->
  Pkgs_ml.Sparse_index.{ name; raw = Data.Json.Object [ ("name", Data.Json.String name) ] }

let make_registry_document = fun ?(releases = []) ~name ~latest () ->
  Pkgs_ml.Sparse_index.{
    schema_version = 1;
    name;
    latest;
    updated_at = "2026-04-01T00:00:00Z";
    releases;
  }

let make_registry = fun packages ->
  Pkgs_ml.Registry.in_memory ~cache:(make_registry_cache ()) ~packages ()

let make_registry_with_releases = fun ~packages ~releases ->
  Pkgs_ml.Registry.in_memory ~cache:(make_registry_cache ()) ~packages ~releases ()

let write_package_manifest = fun ~root contents ->
  Fs.create_dir_all root |> Result.expect ~msg:"expected package root to be created";
  Fs.write contents Path.(root / Path.v "tusk.toml") |> Result.expect ~msg:"expected package manifest to be written"

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let run_lock_deps = fun ?emit ?(registry = make_registry []) ~mode ~existing_lock packages ->
  Tusk_pm.Dep_solver.lock_deps
    ?emit
    ~mode
    ~registry
    ~existing_lock
    packages

let collect_event_names = fun fn ->
  let names = ref [] in
  let emit event =
    names := Tusk_model.Event.name event :: !names
  in
  match fn emit with
  | Ok value -> Ok (value, List.rev !names)
  | Error err -> Error err

let test_lock_deps_projects_workspace_packages = fun () ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = Tusk_model.Package.Workspace } ]
    ~build_dependencies:[ { name = "std"; source = Tusk_model.Package.Workspace } ]
    () in
  match run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg; std_pkg ] with
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

let test_lock_deps_resolves_path_dependencies = fun () ->
  with_tempdir "tusk_pm_path_dep"
    (fun workspace_root ->
      let foo_root = Path.(workspace_root / Path.v "vendor/foo") in
      write_package_manifest ~root:foo_root
        {|
[package]
name = "foo"
version = "1.2.3"
|};
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "foo"; source = Tusk_model.Package.Path (Path.v "../../vendor/foo") }
        ]
        () in
      match run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err -> Error ("expected path dependency locking to succeed: " ^ err)
      | Ok lockfile -> (
          let app_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "app") lockfile.packages
          in
          let foo_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "foo") lockfile.packages
          in
          match app_lock, foo_lock with
          | Some app_lock, Some foo_lock ->
              let expected_root = Path.normalize
                Path.(workspace_root / Path.v "packages/app" / Path.v "../../vendor/foo") in
              if
                List.length lockfile.packages = 2
                && (List.hd app_lock.dependencies).package.name = "foo"
                && Path.equal foo_lock.path expected_root
                && foo_lock.provenance = Tusk_model.Lockfile.Path (Path.v "../../vendor/foo")
              then
                Ok ()
              else
                Error "expected path dependency to resolve to an exact local lock package"
          | _ -> Error "expected app and foo to appear in the lockfile"
        ))

let test_lock_deps_resolves_transitive_path_dependencies = fun () ->
  with_tempdir "tusk_pm_transitive_path_dep"
    (fun workspace_root ->
      let foo_root = Path.(workspace_root / Path.v "vendor/foo") in
      let bar_root = Path.(workspace_root / Path.v "vendor/bar") in
      write_package_manifest ~root:foo_root
        {|
[package]
name = "foo"
version = "1.2.3"

[dependencies]
bar = { path = "../bar" }
|};
      write_package_manifest ~root:bar_root
        {|
[package]
name = "bar"
version = "2.0.0"
|};
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "foo"; source = Tusk_model.Package.Path (Path.v "../../vendor/foo") }
        ]
        () in
      match run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err -> Error ("expected transitive path dependencies to resolve: " ^ err)
      | Ok lockfile -> (
          let foo_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "foo") lockfile.packages
          in
          let bar_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "bar") lockfile.packages
          in
          match foo_lock, bar_lock with
          | Some foo_lock, Some bar_lock ->
              let expected_bar_root = Path.normalize
                Path.(workspace_root / Path.v "vendor/foo" / Path.v "../bar") in
              if
                List.length lockfile.packages = 3
                && (List.hd foo_lock.dependencies).package.name = "bar"
                && Path.equal bar_lock.path expected_bar_root
                && bar_lock.provenance = Tusk_model.Lockfile.Path (Path.v "../bar")
              then
                Ok ()
              else
                Error "expected nested path dependency roots to resolve from the declaring package"
          | _ -> Error "expected both foo and bar lock packages"
        ))

let test_lock_deps_resolves_registry_dependencies_to_exact_versions = fun () ->
  let requirement = Std.Version.parse_requirement ">= 1.2.3" |> Result.expect ~msg:"expected requirement to parse" in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[
      { name = "std"; source = Tusk_model.Package.Registry { version = requirement } }
    ]
    () in
  let registry = make_registry
    [
      make_registry_document
        ~name:"std"
        ~latest:"0.2.0"
        ~releases:[
          make_release ~version:"0.2.0" ~dependencies:[ make_registry_dependency "kernel" ] ();
        ]
        ();
      make_registry_document
        ~name:"kernel"
        ~latest:"1.0.0"
        ~releases:[ make_release ~version:"1.0.0" () ]
        ();
    ] in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected registry dependency locking to succeed: " ^ err)
  | Ok lockfile -> (
      let app_lock =
        List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "app") lockfile.packages
      in
      let std_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "std" && pkg.id.version = Some "0.2.0")
          lockfile.packages
      in
      let kernel_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) ->
            pkg.id.name = "kernel" && pkg.id.version = Some "1.0.0")
          lockfile.packages
      in
      match app_lock, std_lock, kernel_lock with
      | Some app_lock, Some std_lock, Some kernel_lock ->
          let app_dependency_name, app_dependency_version =
            match app_lock.dependencies with
            | [ dep ] -> (dep.package.name, dep.package.version)
            | _ -> ("", None)
          in
          let std_dependency_name =
            match std_lock.dependencies with
            | [ dep ] -> dep.package.name
            | _ -> ""
          in
          if
            List.length lockfile.packages = 3
            && app_dependency_name = "std"
            && app_dependency_version = Some "0.2.0"
            && std_lock.id.version = Some "0.2.0"
            && Path.to_string std_lock.path = "/Users/example/.tusk/registry/pkgs.ml/src/std/0.2.0"
            && std_dependency_name = "kernel"
            && kernel_lock.id.version = Some "1.0.0"
          then
            Ok ()
          else
            Error "expected registry dependency to resolve to exact external lock packages"
      | _ -> Error "expected workspace and transitive registry lock packages"
    )

let test_lock_deps_handles_cyclic_registry_dependencies = fun () ->
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[
      { name = "foo"; source = Tusk_model.Package.Registry { version = Std.Version.any } }
    ]
    () in
  let registry = make_registry
    [
      make_registry_document
        ~name:"foo"
        ~latest:"1.0.0"
        ~releases:[
          make_release ~version:"1.0.0" ~dependencies:[ make_registry_dependency "bar" ] ();
        ]
        ();
      make_registry_document
        ~name:"bar"
        ~latest:"2.0.0"
        ~releases:[
          make_release ~version:"2.0.0" ~dependencies:[ make_registry_dependency "foo" ] ();
        ]
        ();
    ] in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected cyclic registry dependencies to resolve: " ^ err)
  | Ok lockfile -> (
      let foo_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "foo" && pkg.id.version = Some "1.0.0")
          lockfile.packages
      in
      let bar_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "bar" && pkg.id.version = Some "2.0.0")
          lockfile.packages
      in
      match foo_lock, bar_lock with
      | Some foo_lock, Some bar_lock ->
          let foo_dep_name, foo_dep_version =
            match foo_lock.dependencies with
            | [ dep ] -> (dep.package.name, dep.package.version)
            | _ -> ("", None)
          in
          let bar_dep_name, bar_dep_version =
            match bar_lock.dependencies with
            | [ dep ] -> (dep.package.name, dep.package.version)
            | _ -> ("", None)
          in
          if
            List.length lockfile.packages = 3
            && foo_dep_name = "bar"
            && foo_dep_version = Some "2.0.0"
            && bar_dep_name = "foo"
            && bar_dep_version = Some "1.0.0"
          then
            Ok ()
          else
            Error "expected cyclic registry dependencies to terminate with exact cross-links"
      | _ -> Error "expected foo and bar to appear in the cyclic lockfile"
    )

let test_lock_refresh_preserves_existing_registry_version = fun () ->
  let requirement = Std.Version.parse_requirement "*" |> Result.expect ~msg:"expected requirement to parse" in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[
      { name = "std"; source = Tusk_model.Package.Registry { version = requirement } }
    ]
    () in
  let existing_lock =
    Tusk_model.Lockfile.{
      format_version = 1;
      packages =
        [ {
            id = { registry = None; name = "app"; version = None };
            path = Path.v "/workspace/packages/app";
            manifest_path = Path.v "/workspace/packages/app/tusk.toml";
            provenance = Workspace;
            dependencies = [
              {
                name = "std";
                package = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" }
              }
            ];
            build_dependencies = [];
            dev_dependencies = [];
          }; {
            id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0";
            manifest_path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0/tusk.toml";
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          } ];
    }
  in
  let registry = make_registry
    [
      make_registry_document
        ~name:"std"
        ~latest:"0.2.0"
        ~releases:[ make_release ~version:"0.2.0" () ]
        ();
    ] in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err -> Error ("expected refresh lock to preserve registry version: " ^ err)
  | Ok lockfile ->
      let app_lock = List.hd lockfile.packages in
      if
        List.length lockfile.packages = 2
        && (List.hd app_lock.dependencies).package.version = Some "0.1.0"
        && (List.nth lockfile.packages 1).id.version = Some "0.1.0"
      then
        Ok ()
      else
        Error "expected refresh to preserve existing locked registry selections"

let test_lock_refresh_preserves_existing_external_nodes = fun () ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let existing_lock =
    Tusk_model.Lockfile.{
      format_version = 1;
      packages =
        [ {
            id = { registry = None; name = "app"; version = None };
            path = Path.v "/workspace/packages/app";
            manifest_path = Path.v "/workspace/packages/app/tusk.toml";
            provenance = Workspace;
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          }; {
            id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0";
            manifest_path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0/tusk.toml";
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          }; ];
    }
  in
  match run_lock_deps ~mode:Refresh ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err -> Error ("expected refresh lock to preserve existing nodes: " ^ err)
  | Ok lockfile ->
      if
        List.length lockfile.packages = 2
        && (List.nth lockfile.packages 1).id.name = "std"
        && (List.nth lockfile.packages 1).id.version = Some "0.1.0"
      then
        Ok ()
      else
        Error "expected refresh to preserve existing external lock nodes"

let test_unlock_discards_existing_external_nodes = fun () ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let existing_lock =
    Tusk_model.Lockfile.{
      format_version = 1;
      packages =
        [ {
            id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0";
            manifest_path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0/tusk.toml";
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          }; ];
    }
  in
  match run_lock_deps ~mode:Unlock ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err -> Error ("expected unlock to rebuild workspace nodes: " ^ err)
  | Ok lockfile ->
      if List.length lockfile.packages = 1 && (List.hd lockfile.packages).id.name = "app" then
        Ok ()
      else
        Error "expected unlock to discard preserved external lock nodes"

let test_lock_refresh_requires_lock_when_missing = fun () ->
  with_tempdir "tusk_pm_missing_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected manifest write to succeed";
      match Tusk_pm.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok true -> Ok ()
      | Ok false -> Error "expected missing lockfile to require refresh"
      | Error err -> Error err)

let test_lock_refresh_false_when_lock_is_newer = fun () ->
  with_tempdir "tusk_pm_fresh_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected manifest write to succeed";
      sleep (Time.Duration.from_millis 20);
      Fs.write "format_version = 1\npackages = []\n" lock_path |> Result.expect ~msg:"expected lockfile write to succeed";
      match Tusk_pm.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok false -> Ok ()
      | Ok true -> Error "expected newer lockfile to avoid refresh"
      | Error err -> Error err)

let test_lock_refresh_true_when_manifest_is_newer = fun () ->
  with_tempdir "tusk_pm_stale_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected manifest write to succeed";
      Fs.write "format_version = 1\npackages = []\n" lock_path |> Result.expect ~msg:"expected lockfile write to succeed";
      sleep (Time.Duration.from_millis 20);
      Fs.write "[workspace]\nmembers = [\"packages/demo\"]\n" manifest_path |> Result.expect ~msg:"expected manifest rewrite to succeed";
      match Tusk_pm.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok true -> Ok ()
      | Ok false -> Error "expected newer manifest to require refresh"
      | Error err -> Error err)

let test_lockfile_store_roundtrips = fun () ->
  with_tempdir "tusk_pm_lockfile_store"
    (fun workspace_root ->
      let lockfile =
        Tusk_model.Lockfile.{
          format_version = 1;
          packages =
            [ {
                id = { registry = None; name = "app"; version = None };
                path =
                  Path.(workspace_root / Path.v "packages/app");
                manifest_path =
                  Path.(workspace_root / Path.v "packages/app/tusk.toml");
                provenance = Workspace;
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              }; ];
        }
      in
      match Tusk_pm.Lockfile_store.write ~workspace_root lockfile with
      | Error err -> Error ("expected lockfile write to succeed: " ^ err)
      | Ok () -> (
          match Tusk_pm.Lockfile_store.read ~workspace_root with
          | Error err -> Error ("expected lockfile read to succeed: " ^ err)
          | Ok None -> Error "expected written lockfile to exist"
          | Ok (Some reloaded) ->
              if
                reloaded.format_version = 1
                && List.length reloaded.packages = 1
                && (List.hd reloaded.packages).id.name = "app"
              then
                Ok ()
              else
                Error "expected lockfile store roundtrip to preserve package data"
        ))

let test_lockfile_store_returns_none_when_missing = fun () ->
  with_tempdir "tusk_pm_missing_store"
    (fun workspace_root ->
      match Tusk_pm.Lockfile_store.read ~workspace_root with
      | Ok None -> Ok ()
      | Ok (Some _) -> Error "expected missing lockfile to return none"
      | Error err -> Error err)

let test_lockfile_store_bubbles_parse_errors = fun () ->
  with_tempdir "tusk_pm_invalid_lockfile"
    (fun workspace_root ->
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "not = [valid\n" lock_path |> Result.expect ~msg:"expected invalid lockfile write to succeed";
      match Tusk_pm.Lockfile_store.read ~workspace_root with
      | Ok _ -> Error "expected invalid lockfile to fail"
      | Error err ->
          if
            String.contains err "failed to parse lockfile TOML" || String.contains err "failed to decode lockfile"
          then
            Ok ()
          else
            Error ("unexpected error: " ^ err))

let test_ensure_lock_refreshes_missing_lock_and_resolves_workspace = fun () ->
  with_tempdir "tusk_pm_ensure_lock_missing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let std_pkg = make_package ~name:"std" ~path:Path.(workspace_root / Path.v "packages/std") () in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = Tusk_model.Package.Workspace } ]
        () in
      match collect_event_names
        (fun emit ->
          Tusk_pm.ensure_lock
            ~emit
            ~mode:Tusk_pm.Dep_solver.Refresh
            ~registry:(make_registry [])
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg; std_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to refresh missing lock: " ^ err)
      | Ok ((lockfile, resolved), event_names) ->
          let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
          if
            List.length lockfile.packages = 2
            && List.length resolved = 2
            && List.mem "tusk.pm.lockfile.read.started" event_names
            && List.mem "tusk.pm.lockfile.read.finished" event_names
            && List.mem "tusk.pm.resolution.started" event_names
            && List.mem "tusk.pm.resolution.refreshing_lock" event_names
            && List.mem "tusk.pm.lockfile.write.started" event_names
            && List.mem "tusk.pm.lockfile.write.finished" event_names
            && List.mem "tusk.pm.resolution.finished" event_names
            && Result.unwrap_or ~default:false (Fs.exists lock_path)
          then
            Ok ()
          else
            Error "expected ensure_lock to write a fresh lockfile and emit PM lifecycle events")

let test_ensure_lock_uses_existing_fresh_lock = fun () ->
  with_tempdir "tusk_pm_ensure_lock_existing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let std_pkg = make_package ~name:"std" ~path:Path.(workspace_root / Path.v "packages/std") () in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = Tusk_model.Package.Workspace } ]
        () in
      sleep (Time.Duration.from_millis 20);
      let existing_lock = run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg; std_pkg ]
      |> Result.expect ~msg:"expected workspace lock projection to succeed" in
      Tusk_pm.Lockfile_store.write ~workspace_root existing_lock |> Result.expect ~msg:"expected initial lockfile to be written";
      match collect_event_names
        (fun emit ->
          Tusk_pm.ensure_lock
            ~emit
            ~mode:Tusk_pm.Dep_solver.Refresh
            ~registry:(make_registry [])
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg; std_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to use existing lock: " ^ err)
      | Ok ((lockfile, resolved), event_names) ->
          if
            List.length lockfile.packages = 2
            && List.length resolved = 2
            && List.mem "tusk.pm.resolution.using_existing_lock" event_names
            && not (List.mem "tusk.pm.lockfile.write.started" event_names)
          then
            Ok ()
          else
            Error "expected ensure_lock to reuse a fresh existing lock without rewriting it")

let test_ensure_lock_materializes_registry_packages_before_projection = fun () ->
  with_tempdir "tusk_pm_ensure_lock_materializes"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let requirement = Std.Version.parse_requirement "*" |> Result.expect ~msg:"expected requirement to parse" in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "std"; source = Tusk_model.Package.Registry { version = requirement } }
        ]
        () in
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let registry = Pkgs_ml.Registry.in_memory
        ~cache:registry_cache
        ~packages:[
          make_registry_document
            ~name:"std"
            ~latest:"0.2.0"
            ~releases:[ make_release ~version:"0.2.0" () ]
            ();
        ]
        ~releases:[
          {
            Pkgs_ml.Registry.package_name = "std";
            version = "0.2.0";
            manifest_toml = "[package]\nname = \"std\"\n";
            files = [ { path = Path.v "src/std.ml"; contents = "let answer = 42\n" } ]
          };
        ]
        ()
      in
      match collect_event_names
        (fun emit ->
          Tusk_pm.ensure_lock
            ~emit
            ~mode:Tusk_pm.Dep_solver.Refresh
            ~registry
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to materialize registry packages: " ^ err)
      | Ok ((_, resolved), event_names) ->
          let manifest_path = Pkgs_ml.Registry_cache.package_src_dir
            registry_cache
            ~package_name:"std"
            ~version:"0.2.0"
          |> fun root -> Path.(root / Path.v "tusk.toml") in
          if
            List.length resolved = 2
            && Result.unwrap_or ~default:false (Fs.exists manifest_path)
            && List.mem "tusk.pm.universe.building" event_names
            && List.mem "tusk.pm.universe.built" event_names
            && List.mem "tusk.pm.package_metadata.fetch.started" event_names
            && List.mem "tusk.pm.package_metadata.fetch.finished" event_names
            && List.mem "tusk.pm.package_materialization.started" event_names
            && List.mem "tusk.pm.package_materialization.finished" event_names
            && List.mem "tusk.pm.package_manifest.fetch.started" event_names
            && List.mem "tusk.pm.package_manifest.fetch.finished" event_names
            && List.mem "tusk.pm.package_resolved_for_build" event_names
          then
            Ok ()
          else
            Error "expected ensure_lock to materialize external package manifests before projection")

let test_ensure_lock_reuses_existing_lock_and_materializes_missing_registry_packages = fun () ->
  with_tempdir "tusk_pm_ensure_lock_materializes_existing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let requirement = Std.Version.parse_requirement "*" |> Result.expect ~msg:"expected requirement to parse" in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "std"; source = Tusk_model.Package.Registry { version = requirement } }
        ]
        () in
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let registry = Pkgs_ml.Registry.in_memory
        ~cache:registry_cache
        ~packages:[
          make_registry_document
            ~name:"std"
            ~latest:"0.2.0"
            ~releases:[ make_release ~version:"0.2.0" () ]
            ();
        ]
        ~releases:[
          {
            Pkgs_ml.Registry.package_name = "std";
            version = "0.2.0";
            manifest_toml = "[package]\nname = \"std\"\n";
            files = []
          };
        ]
        ()
      in
      let existing_lock = Tusk_pm.Dep_solver.lock_deps
        ~mode:Tusk_pm.Dep_solver.Refresh
        ~registry
        ~existing_lock:None [ app_pkg ]
      |> Result.expect ~msg:"expected initial lock solve to succeed" in
      Tusk_pm.Lockfile_store.write ~workspace_root existing_lock |> Result.expect ~msg:"expected initial lockfile write to succeed";
      match collect_event_names
        (fun emit ->
          Tusk_pm.ensure_lock
            ~emit
            ~mode:Tusk_pm.Dep_solver.Refresh
            ~registry
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to reuse lock and materialize missing packages: "
      ^ err)
      | Ok ((_, resolved), event_names) ->
          if
            List.length resolved = 2
            && List.mem "tusk.pm.resolution.using_existing_lock" event_names
            && List.mem "tusk.pm.package_materialization.finished" event_names
          then
            Ok ()
          else
            Error "expected ensure_lock to reuse the lock while still materializing missing registry packages")

let test_ensure_workspace_projects_materialized_registry_packages = fun () ->
  with_tempdir "tusk_pm_ensure_workspace"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = [\"packages/app\"]\n" workspace_manifest
      |> Result.expect ~msg:"expected workspace manifest to be written";
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "std"; source = Tusk_model.Package.Registry { version = Std.Version.any } }
        ]
        () in
      let app_pkg = { app_pkg with relative_path = Path.v "packages/app" } in
      let workspace = Tusk_model.Workspace.make ~root:workspace_root ~packages:[ app_pkg ] () in
      let registry = Pkgs_ml.Registry.in_memory
        ~cache:registry_cache
        ~packages:[
          make_registry_document
            ~name:"std"
            ~latest:"0.2.0"
            ~releases:[ make_release ~version:"0.2.0" () ]
            ();
        ]
        ~releases:[ {
          Pkgs_ml.Registry.package_name = "std";
          version = "0.2.0";
          manifest_toml =
            {|
[package]
name = "std"
version = "0.2.0"
|};
          files = [];
        } ]
        ()
      in
      match Tusk_pm.ensure_workspace
        ~mode:Tusk_pm.Dep_solver.Refresh
        ~registry
        ~workspace
        () with
      | Error err -> Error ("expected ensure_workspace to succeed: " ^ err)
      | Ok resolved_workspace ->
          let std_pkg =
            List.find_opt
              (fun (pkg: Tusk_model.Package.t) ->
                String.equal pkg.name "std")
              resolved_workspace.packages
          in
          let expected_std_root = Pkgs_ml.Registry_cache.package_src_dir
            registry_cache
            ~package_name:"std"
            ~version:"0.2.0" in
          match std_pkg with
          | Some std_pkg ->
              if
                List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) resolved_workspace.packages
                = [ "app"; "std" ]
                && Path.equal std_pkg.path expected_std_root
              then
                Ok ()
              else
                Error "expected ensure_workspace to return a build-ready workspace with registry packages"
          | None -> Error "expected ensure_workspace to project std into the workspace")

let test_projection_resolves_workspace_packages = fun () ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = Tusk_model.Package.Workspace } ]
    () in
  let lockfile = run_lock_deps
    ~mode:Tusk_pm.Dep_solver.Refresh
    ~existing_lock:None [ app_pkg; std_pkg ]
  |> Result.expect ~msg:"expected lock projection to succeed" in
  match Tusk_pm.Projection.resolve_packages ~packages:[ app_pkg; std_pkg ] ~lockfile () with
  | Error err -> Error ("expected projection to resolve workspace packages: " ^ err)
  | Ok resolved ->
      let app = List.hd resolved in
      if
        List.length resolved = 2
        && app.id.name = "app"
        && List.length app.runtime_resolved = 1
        && (List.hd app.runtime_resolved).resolved_id.name = "std"
      then
        Ok ()
      else
        Error "expected projection to preserve resolved runtime dependency ids"

let test_projection_loads_external_manifests_from_lockfile = fun () ->
  with_tempdir "tusk_pm_projection_external"
    (fun workspace_root ->
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "std"; source = Tusk_model.Package.Registry { version = Std.Version.any } }
        ]
        () in
      let std_root = Path.(workspace_root / Path.v ".tusk/registry/pkgs.ml/src/std/0.2.0") in
      let kernel_root = Path.(workspace_root / Path.v ".tusk/registry/pkgs.ml/src/kernel/1.0.0") in
      let std_manifest_path = Path.(std_root / Path.v "tusk.toml") in
      let kernel_manifest_path = Path.(kernel_root / Path.v "tusk.toml") in
      Fs.create_dir_all std_root |> Result.expect ~msg:"expected std root to be created";
      Fs.create_dir_all kernel_root |> Result.expect ~msg:"expected kernel root to be created";
      Fs.write
        {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = "*"
|}
        std_manifest_path |> Result.expect ~msg:"expected std manifest to be written";
      Fs.write
        {|
[package]
name = "kernel"
version = "1.0.0"
|}
        kernel_manifest_path |> Result.expect ~msg:"expected kernel manifest to be written";
      let lockfile =
        Tusk_model.Lockfile.{
          format_version = 1;
          packages =
            [ {
                id = { registry = None; name = "app"; version = None };
                path = app_pkg.path;
                manifest_path =
                  Path.(app_pkg.path / Path.v "tusk.toml");
                provenance = Workspace;
                dependencies = [
                  {
                    name = "std";
                    package = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" }
                  }
                ];
                build_dependencies = [];
                dev_dependencies = [];
              }; {
                id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" };
                path = std_root;
                manifest_path = std_manifest_path;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [
                  {
                    name = "kernel";
                    package = { registry = Some "pkgs.ml"; name = "kernel"; version = Some "1.0.0" }
                  }
                ];
                build_dependencies = [];
                dev_dependencies = [];
              }; {
                id = { registry = Some "pkgs.ml"; name = "kernel"; version = Some "1.0.0" };
                path = kernel_root;
                manifest_path = kernel_manifest_path;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              } ];
        }
      in
      match collect_event_names
        (fun emit ->
          Tusk_pm.Projection.resolve_packages
            ~emit
            ~packages:[ app_pkg ]
            ~lockfile
            ()) with
      | Error err -> Error ("expected projection to load external manifests: " ^ err)
      | Ok (resolved, event_names) ->
          let std_resolved =
            List.find_opt
              (fun (pkg: Tusk_model.Package.resolved) ->
                pkg.id.name = "std" && pkg.id.version = Some "0.2.0")
              resolved
          in
          let kernel_resolved =
            List.find_opt
              (fun (pkg: Tusk_model.Package.resolved) ->
                pkg.id.name = "kernel" && pkg.id.version = Some "1.0.0")
              resolved
          in
          match std_resolved, kernel_resolved with
          | Some std_resolved, Some kernel_resolved ->
              if
                List.length resolved = 3
                && List.mem "tusk.pm.package_manifest.fetch.started" event_names
                && List.mem "tusk.pm.package_manifest.fetch.finished" event_names
                && List.mem "tusk.pm.package_resolved_for_build" event_names
                && Path.to_string std_resolved.materialized_root = Path.to_string std_root
                && List.length std_resolved.runtime_resolved = 1
                && (List.hd std_resolved.runtime_resolved).resolved_id.name = "kernel"
                && Path.to_string kernel_resolved.materialized_root = Path.to_string kernel_root
              then
                Ok ()
              else
                Error "expected projection to include external lockfile packages"
          | _ -> Error "expected projection to resolve both std and kernel from external manifests")

let test_projection_bubbles_external_manifest_errors = fun () ->
  with_tempdir "tusk_pm_projection_manifest_error"
    (fun workspace_root ->
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "std"; source = Tusk_model.Package.Registry { version = Std.Version.any } }
        ]
        () in
      let std_root = Path.(workspace_root / Path.v ".tusk/registry/pkgs.ml/src/std/0.2.0") in
      let std_manifest_path = Path.(std_root / Path.v "tusk.toml") in
      Fs.create_dir_all std_root |> Result.expect ~msg:"expected std root to be created";
      Fs.write
        {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = 123
|}
        std_manifest_path |> Result.expect ~msg:"expected invalid std manifest to be written";
      let lockfile =
        Tusk_model.Lockfile.{
          format_version = 1;
          packages =
            [ {
                id = { registry = None; name = "app"; version = None };
                path = app_pkg.path;
                manifest_path =
                  Path.(app_pkg.path / Path.v "tusk.toml");
                provenance = Workspace;
                dependencies = [
                  {
                    name = "std";
                    package = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" }
                  }
                ];
                build_dependencies = [];
                dev_dependencies = [];
              }; {
                id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" };
                path = std_root;
                manifest_path = std_manifest_path;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              } ];
        }
      in
      match Tusk_pm.Projection.resolve_packages ~packages:[ app_pkg ] ~lockfile () with
      | Ok _ -> Error "expected invalid external manifest to fail projection"
      | Error err ->
          if
            String.contains err "must be a string or table" || String.contains err "failed to decode package manifest"
          then
            Ok ()
          else
            Error ("unexpected projection error: " ^ err))

let test_projection_fails_when_lockfile_is_missing_package = fun () ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let lockfile = Tusk_model.Lockfile.{ format_version = 1; packages = [] } in
  match Tusk_pm.Projection.resolve_packages ~packages:[ app_pkg ] ~lockfile () with
  | Ok _ -> Error "expected projection to fail when lockfile is missing package"
  | Error err ->
      if String.contains err "lockfile is missing package 'app'" then
        Ok ()
      else
        Error ("unexpected error: " ^ err)

let tests =
  Test.[
    case "dep solver: projects workspace packages into lockfile" test_lock_deps_projects_workspace_packages;
    case "dep solver: resolves path dependencies" test_lock_deps_resolves_path_dependencies;
    case "dep solver: resolves transitive path dependencies" test_lock_deps_resolves_transitive_path_dependencies;
    case "dep solver: resolves registry dependencies to exact versions" test_lock_deps_resolves_registry_dependencies_to_exact_versions;
    case "dep solver: handles cyclic registry dependencies" test_lock_deps_handles_cyclic_registry_dependencies;
    case "dep solver: refresh preserves existing registry versions" test_lock_refresh_preserves_existing_registry_version;
    case "dep solver: refresh preserves existing external nodes" test_lock_refresh_preserves_existing_external_nodes;
    case "dep solver: unlock discards existing external nodes" test_unlock_discards_existing_external_nodes;
    case "lock refresh: missing lock requires refresh" test_lock_refresh_requires_lock_when_missing;
    case "lock refresh: newer lock avoids refresh" test_lock_refresh_false_when_lock_is_newer;
    case "lock refresh: newer manifest requires refresh" test_lock_refresh_true_when_manifest_is_newer;
    case "lockfile store: roundtrips root lockfile" test_lockfile_store_roundtrips;
    case "lockfile store: missing lockfile returns none" test_lockfile_store_returns_none_when_missing;
    case "lockfile store: bubbles parse errors" test_lockfile_store_bubbles_parse_errors;
    case "ensure lock: refreshes missing lock and resolves workspace graph" test_ensure_lock_refreshes_missing_lock_and_resolves_workspace;
    case "ensure lock: uses existing fresh lock" test_ensure_lock_uses_existing_fresh_lock;
    case "ensure lock: materializes registry packages before projection" test_ensure_lock_materializes_registry_packages_before_projection;
    case "ensure lock: reuses existing lock and materializes missing registry packages" test_ensure_lock_reuses_existing_lock_and_materializes_missing_registry_packages;
    case "ensure workspace: projects materialized registry packages" test_ensure_workspace_projects_materialized_registry_packages;
    case "projection: resolves workspace packages from lockfile" test_projection_resolves_workspace_packages;
    case "projection: loads external manifests from lockfile" test_projection_loads_external_manifests_from_lockfile;
    case "projection: bubbles external manifest errors" test_projection_bubbles_external_manifest_errors;
    case "projection: fails when lockfile is missing package" test_projection_fails_when_lockfile_is_missing_package;
  ]

let name = "Tusk PM Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
