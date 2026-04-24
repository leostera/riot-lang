open Std
module Test = Std.Test

let version = Std.Version.make ~major:0 ~minor:1 ~patch:0 ()

let package_name = fun name ->
  Riot_model.Package_name.from_string name |> Result.expect ~msg:("invalid package name: " ^ name)

let make_registry = fun () ->
  let cache = Pkgs_ml.Registry_cache.create
    ~riot_home:(Path.v "/tmp/riot-publish-tests")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected in-memory registry cache" in
  Pkgs_ml.Registry.in_memory ~cache ~packages:[] ()

let make_sources = fun () ->
  Riot_model.Package.{
    src = [];
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let make_package = fun ~workspace_root ?(public = true) name ->
  let package_root = Path.(workspace_root / Path.v name) in
  let publish =
    Riot_model.Package.{
      version = Some version;
      description = Some ("Package " ^ name);
      license = Some "Apache-2.0";
      is_public = Some public
    } in
  Riot_model.Package.make
    ~name:(package_name name)
    ~path:package_root
    ~relative_path:(Path.v name)
    ~sources:(make_sources ())
    ~publish
    ()

let make_workspace = fun packages ->
  let root = Path.v "/workspace" in
  Riot_model.Workspace.make_realized ~root ~packages ()

let panic_unexpected = fun label -> panic ("unexpected publish dependency call: " ^ label)

let make_deps = fun ?(workspace_publish_order = fun ~packages -> Ok packages) ?(published_version_exists = fun ~registry:_ ~package_name:_ ~version:_ ->
  Ok false) () ->
  Riot_publish.For_test.{
    resolve_registry = (fun () -> Ok (make_registry ()));
    load_api_token = (fun ~registry_name:_ -> Ok "token");
    workspace_publish_order;
    published_version_exists;
    run_fmt_check = (fun ~emit:_ ~workspace:_ ~package:_ -> panic_unexpected "fmt");
    run_fix_check = (fun ~emit:_ ~registry:_ ~workspace:_ ~request:_ ~package:_ -> panic_unexpected "fix");
    run_build_check = (fun ~emit:_ ~workspace:_ ~package_name:_ ~profile:_ -> panic_unexpected "build");
    plan_publish = (fun ~registry:_ ~publishing_workspace_packages:_ ~package:_ -> panic_unexpected "metadata");
    prepare_publish_artifact = (fun ~target_dir_root:_ _ -> panic_unexpected "prepare");
    publish_prepared = (fun ~registry:_ ~api_token:_ _ -> panic_unexpected "publish");
  }

let event_name = fun event ->
  match event with
  | Riot_publish.SkippedNotPublic { package; _ } -> "skipped-not-public:"
  ^ Riot_model.Package_name.to_string package
  | Riot_publish.SkippedAlreadyPublished { package; _ } -> "skipped-already-published:"
  ^ Riot_model.Package_name.to_string package
  | Riot_publish.CheckStarted { package; stage; _ } ->
      "started:" ^ Riot_model.Package_name.to_string package ^ ":" ^ (
        match stage with
        | `fmt -> "fmt"
        | `fix -> "fix"
        | `build -> "build"
        | `metadata -> "metadata"
      )
  | Riot_publish.CheckFinished { package; stage; _ } ->
      "finished:" ^ Riot_model.Package_name.to_string package ^ ":" ^ (
        match stage with
        | `fmt -> "fmt"
        | `fix -> "fix"
        | `build -> "build"
        | `metadata -> "metadata"
      )
  | Riot_publish.Packing { package; _ } -> "packing:" ^ Riot_model.Package_name.to_string package
  | Riot_publish.DryRunPlanned prepared -> "dry-run:"
  ^ Riot_model.Package_name.to_string prepared.package.name
  | Riot_publish.PackagePublished published -> "published:" ^ published.package_name
  | Riot_publish.Fmt _ -> "fmt-event"
  | Riot_publish.Fix _ -> "fix-event"
  | Riot_publish.Build _ -> "build-event"

let test_workspace_without_packages_errors = fun _ctx ->
  let workspace = make_workspace [] in
  match Riot_publish.For_test.publish_with
    ~deps:(make_deps ())
    ~workspace
    ~request:Riot_publish.{ selection = Workspace; skip_check = false }
    ~mode:DryRun () with
  | Error Riot_publish.NoWorkspacePackages -> Ok ()
  | Ok _ -> Error "expected workspace publish without packages to fail"
  | Error err -> Error ("unexpected publish error: " ^ Riot_publish.publish_error_message err)

let test_missing_package_errors = fun _ctx ->
  let workspace = make_workspace [ make_package ~workspace_root:(Path.v "/workspace") "demo" ] in
  match Riot_publish.For_test.publish_with
    ~deps:(make_deps ())
    ~workspace
    ~request:Riot_publish.{ selection = Package (package_name "missing"); skip_check = false }
    ~mode:DryRun () with
  | Error (Riot_publish.PackageNotFound { package }) when Riot_model.Package_name.equal
    package
    (package_name "missing") -> Ok ()
  | Ok _ -> Error "expected missing package selection to fail"
  | Error err -> Error ("unexpected publish error: " ^ Riot_publish.publish_error_message err)

let test_private_package_is_skipped = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let private_pkg = make_package ~workspace_root ~public:false "private-lib" in
  let events = ref [] in
  let workspace = make_workspace [ private_pkg ] in
  match Riot_publish.For_test.publish_with
    ~on_event:(fun event -> events := event_name event :: !events)
    ~deps:(make_deps ())
    ~workspace
    ~request:Riot_publish.{ selection = Package (package_name "private-lib"); skip_check = false }
    ~mode:DryRun () with
  | Error err -> Error ("expected private package to be skipped, got error: "
  ^ Riot_publish.publish_error_message err)
  | Ok outcomes ->
      if not (List.is_empty outcomes) then
        Error "expected no publish outcomes for a private package"
      else if not (List.reverse !events = [ "skipped-not-public:private-lib" ]) then
        Error ("unexpected events: " ^ String.concat ", " (List.reverse !events))
      else
        Ok ()

let test_workspace_selection_orders_public_packages_only = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let public_a = make_package ~workspace_root "public-a" in
  let private_pkg = make_package ~workspace_root ~public:false "private-lib" in
  let public_b = make_package ~workspace_root "public-b" in
  let ordered = ref [] in
  let deps =
    make_deps
      ~workspace_publish_order:(fun ~packages ->
        ordered := List.map packages ~fn:(fun (pkg: Riot_model.Package.t) -> pkg.name);
        Ok (List.reverse packages))
      ~published_version_exists:(fun ~registry:_ ~package_name:_ ~version:_ -> Ok true)
      ()
  in
  let workspace = make_workspace [ public_a; private_pkg; public_b ] in
  match Riot_publish.For_test.publish_with
    ~deps
    ~workspace
    ~request:Riot_publish.{ selection = Workspace; skip_check = false }
    ~mode:DryRun () with
  | Error err -> Error ("expected workspace selection to succeed, got error: "
  ^ Riot_publish.publish_error_message err)
  | Ok outcomes ->
      let ordered_names = !ordered in
      let outcome_names =
        List.filter_map outcomes
          ~fn:(fun outcome ->
            match outcome with
            | Riot_publish.Skipped { package; _ } -> Some package
            | _ -> None)
      in
      if not (ordered_names = [ package_name "public-a"; package_name "public-b" ]) then
        Error ("unexpected ordered package set: "
        ^ String.concat ", " (List.map ordered_names ~fn:Riot_model.Package_name.to_string))
      else if not (outcome_names = [ package_name "public-b"; package_name "public-a" ]) then
        Error ("unexpected publish outcomes: "
        ^ String.concat ", " (List.map outcome_names ~fn:Riot_model.Package_name.to_string))
      else
        Ok ()

let test_publish_error_message_renders_typed_registry_initialization_error = fun _ctx ->
  let message = Riot_publish.publish_error_message
    (Riot_publish.RegistryInitializationFailed {
      registry_name = "pkgs.ml";
      error = Riot_deps.RegistryFilesystemInitializationFailed Pkgs_ml.Registry_cache.HomeDirectoryUnavailable
    }) in
  let expected = "failed to initialize registry 'pkgs.ml': failed to determine home directory for pkgs.ml cache" in
  if String.equal message expected then
    Ok ()
  else
    Error ("unexpected registry initialization message: " ^ message)

let test_publish_error_message_renders_typed_workspace_errors = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let scan_message = Riot_publish.publish_error_message
    (Riot_publish.WorkspaceScanFailed {
      workspace_root;
      error = Riot_model.Workspace_manager.NoWorkspaceRootFound
    }) in
  let load_message = Riot_publish.publish_error_message
    (Riot_publish.WorkspaceLoadHadErrors {
      workspace_root;
      errors = [
        Riot_model.Workspace_manager.PackageTomlReadFailed {
          package = "dep";
          path = "deps/dep/riot.toml"
        }
      ]
    }) in
  let prepare_message = Riot_publish.publish_error_message
    (Riot_publish.WorkspacePrepareFailed {
      workspace_root;
      error = Riot_model.Pm_error.Unexpected { error = "lock refresh failed" }
    }) in
  let expected_scan = "failed to scan workspace '/workspace': no workspace root found" in
  let expected_load = "failed to load workspace '/workspace': package 'dep': failed to read riot.toml at path deps/dep/riot.toml" in
  let expected_prepare = "failed to prepare workspace '/workspace': lock refresh failed" in
  if not (String.equal scan_message expected_scan) then
    Error ("unexpected workspace scan message: " ^ scan_message)
  else if not (String.equal load_message expected_load) then
    Error ("unexpected workspace load message: " ^ load_message)
  else if not (String.equal prepare_message expected_prepare) then
    Error ("unexpected workspace prepare message: " ^ prepare_message)
  else
    Ok ()

let test_publish_error_message_renders_typed_build_check_error = fun _ctx ->
  let package = package_name "demo" in
  let message = Riot_publish.publish_error_message
    (Riot_publish.BuildCheckFailed {
      package;
      error = Riot_build.UnexpectedError { reason = "compiler unavailable" }
    }) in
  let expected = "'riot build' failed for package 'demo': compiler unavailable" in
  if String.equal message expected then
    Ok ()
  else
    Error ("unexpected build check message: " ^ message)

let test_publish_error_message_renders_check_exceptions = fun _ctx ->
  let package = package_name "demo" in
  let fmt_message = Riot_publish.publish_error_message
    (Riot_publish.FmtCheckFailed { package; error = Failure "formatting failed" }) in
  let fix_message = Riot_publish.publish_error_message
    (Riot_publish.FixCheckFailed { package; error = Failure "lint failed" }) in
  let expected_fmt = "'riot fmt --check' failed for package 'demo': formatting failed" in
  let expected_fix = "'riot fix --check' failed for package 'demo': lint failed" in
  if not (String.equal fmt_message expected_fmt) then
    Error ("unexpected fmt check message: " ^ fmt_message)
  else if not (String.equal fix_message expected_fix) then
    Error ("unexpected fix check message: " ^ fix_message)
  else
    Ok ()

let tests =
  Test.[
    case "publish selection: workspace without packages errors" test_workspace_without_packages_errors;
    case "publish selection: missing package errors" test_missing_package_errors;
    case "publish selection: private package is skipped" test_private_package_is_skipped;
    case "publish selection: workspace orders public packages only" test_workspace_selection_orders_public_packages_only;
    case "publish selection: renders typed registry initialization errors" test_publish_error_message_renders_typed_registry_initialization_error;
    case "publish selection: renders typed workspace errors" test_publish_error_message_renders_typed_workspace_errors;
    case "publish selection: renders typed build check errors" test_publish_error_message_renders_typed_build_check_error;
    case "publish selection: renders check exceptions" test_publish_error_message_renders_check_exceptions;
  ]

let name = "Riot Publish Selection Tests"

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
