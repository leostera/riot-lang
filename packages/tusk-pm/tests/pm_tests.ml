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
  Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/Users/example/.tusk")
    ~registry_name:"pkgs.ml"
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
  Pkgs_ml.Registry.in_memory ~packages ()

let run_lock_deps = fun ?(registry = make_registry []) ?(registry_cache = make_registry_cache ()) ~mode ~existing_lock packages ->
  Tusk_pm.Dep_solver.lock_deps
    ~mode
    ~registry
    ~registry_cache
    ~registry_name:"pkgs.ml"
    ~existing_lock
    packages

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

let test_lock_deps_rejects_path_dependencies_for_now = fun () ->
  let vendor_pkg = make_package
    ~name:"vendor-consumer"
    ~path:(Path.v "/workspace/packages/vendor-consumer")
    ~dependencies:[ { name = "foo"; source = Tusk_model.Package.Path (Path.v "../vendor/foo") } ]
    () in
  match run_lock_deps ~mode:Refresh ~existing_lock:None [ vendor_pkg ] with
  | Ok _ -> Error "expected path dependencies to fail until materialization exists"
  | Error err ->
      if String.contains err "path dependencies are not implemented in tusk-pm yet" then
        Ok ()
      else
        Error ("unexpected error: " ^ err)

let test_lock_deps_resolves_registry_dependencies_to_exact_versions = fun () ->
  let requirement = Std.Version.parse_requirement ">= 1.2.3" |> Result.expect ~msg:"expected requirement to parse" in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[
      { name = "std"; source = Tusk_model.Package.Registry { version = requirement } }
    ]
    () in
  let registry =
    make_registry
      [
        make_registry_document
          ~name:"std"
          ~latest:"0.2.0"
          ~releases:[
            make_release
              ~version:"0.2.0"
              ~dependencies:[ make_registry_dependency "kernel" ]
              ();
          ]
          ();
        make_registry_document
          ~name:"kernel"
          ~latest:"1.0.0"
          ~releases:[ make_release ~version:"1.0.0" () ]
          ();
      ]
  in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected registry dependency locking to succeed: " ^ err)
  | Ok lockfile -> (
      let app_lock =
        List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "app") lockfile.packages
      in
      let std_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) ->
            pkg.id.name = "std" && pkg.id.version = Some "0.2.0")
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

let test_lock_refresh_preserves_existing_registry_version = fun () ->
  let requirement = Std.Version.parse_requirement "*" |> Result.expect ~msg:"expected requirement to parse" in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = Tusk_model.Package.Registry { version = requirement } } ]
    ()
  in
  let existing_lock =
    Tusk_model.Lockfile.{
      format_version = 1;
      packages =
        [ {
            id = { registry = None; name = "app"; version = None };
            path = Path.v "/workspace/packages/app";
            manifest_path = Path.v "/workspace/packages/app/tusk.toml";
            provenance = Workspace;
            dependencies = [ {
              name = "std";
              package = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            } ];
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
  let registry =
    make_registry
      [
        make_registry_document
          ~name:"std"
          ~latest:"0.2.0"
          ~releases:[ make_release ~version:"0.2.0" () ]
          ();
      ]
  in
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

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

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
  match Tusk_pm.Projection.resolve_packages ~packages:[ app_pkg; std_pkg ] ~lockfile with
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

let test_projection_fails_when_lockfile_is_missing_package = fun () ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let lockfile = Tusk_model.Lockfile.{ format_version = 1; packages = [] } in
  match Tusk_pm.Projection.resolve_packages ~packages:[ app_pkg ] ~lockfile with
  | Ok _ -> Error "expected projection to fail when lockfile is missing package"
  | Error err ->
      if String.contains err "lockfile is missing package 'app'" then
        Ok ()
      else
        Error ("unexpected error: " ^ err)

let tests =
  Test.[
    case "dep solver: projects workspace packages into lockfile" test_lock_deps_projects_workspace_packages;
    case "dep solver: rejects path dependencies for now" test_lock_deps_rejects_path_dependencies_for_now;
    case "dep solver: resolves registry dependencies to exact versions" test_lock_deps_resolves_registry_dependencies_to_exact_versions;
    case "dep solver: refresh preserves existing registry versions" test_lock_refresh_preserves_existing_registry_version;
    case "dep solver: refresh preserves existing external nodes" test_lock_refresh_preserves_existing_external_nodes;
    case "dep solver: unlock discards existing external nodes" test_unlock_discards_existing_external_nodes;
    case "lock refresh: missing lock requires refresh" test_lock_refresh_requires_lock_when_missing;
    case "lock refresh: newer lock avoids refresh" test_lock_refresh_false_when_lock_is_newer;
    case "lock refresh: newer manifest requires refresh" test_lock_refresh_true_when_manifest_is_newer;
    case "lockfile store: roundtrips root lockfile" test_lockfile_store_roundtrips;
    case "lockfile store: missing lockfile returns none" test_lockfile_store_returns_none_when_missing;
    case "lockfile store: bubbles parse errors" test_lockfile_store_bubbles_parse_errors;
    case "projection: resolves workspace packages from lockfile" test_projection_resolves_workspace_packages;
    case "projection: fails when lockfile is missing package" test_projection_fails_when_lockfile_is_missing_package;
  ]

let name = "Tusk PM Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
