open Std
module Test = Std.Test
module HashSet = Std.Collections.HashSet

let parse_build = fun args ->
  match ArgParser.get_matches Riot_cli.Build.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_run = fun args ->
  match ArgParser.get_matches Riot_cli.Run.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_test = fun args ->
  match ArgParser.get_matches Riot_cli.Test_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_bench = fun args ->
  match ArgParser.get_matches Riot_cli.Bench_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_install = fun args ->
  match ArgParser.get_matches Riot_cli.Install.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_info = fun args ->
  match ArgParser.get_matches Riot_cli.Info_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_build_accepts_multiple_packages = fun _ctx ->
  match parse_build [ "build"; "syn"; "krasny"; "riot-cli" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      let actual = ArgParser.get_many matches "package" in
      Test.assert_equal ~expected:[ "syn"; "krasny"; "riot-cli" ] ~actual;
      Ok ()

let test_build_usage_shows_variadic_packages = fun _ctx ->
  let usage = ArgParser.usage_string Riot_cli.Build.command in
  if String.contains usage "package..." then
    Ok ()
  else
    Error ("expected variadic package usage, got: " ^ usage)

let test_build_accepts_json_flag = fun _ctx ->
  match parse_build [ "build"; "--json"; "syn" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let test_build_accepts_release_flag = fun _ctx ->
  match parse_build [ "build"; "--release"; "syn" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected --release flag to be parsed"

let test_test_accepts_json_flag = fun _ctx ->
  match parse_test [ "test"; "--json"; "-p"; "riot-build" ] with
  | Error err -> Error ("expected test args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected test --json flag to be parsed"

let test_test_accepts_release_flag = fun _ctx ->
  match parse_test [ "test"; "--release"; "-p"; "riot-build" ] with
  | Error err -> Error ("expected test args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected test --release flag to be parsed"

let test_test_accepts_list_flag = fun _ctx ->
  match parse_test [ "test"; "--list"; "-p"; "riot-build" ] with
  | Error err -> Error ("expected test args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" then
        Ok ()
      else
        Error "expected test --list flag to be parsed"

let test_bench_accepts_json_flag = fun _ctx ->
  match parse_bench [ "bench"; "--json"; "-p"; "std" ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected bench --json flag to be parsed"

let test_bench_accepts_release_flag = fun _ctx ->
  match parse_bench [ "bench"; "--release"; "-p"; "std" ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected bench --release flag to be parsed"

let test_bench_accepts_list_flag = fun _ctx ->
  match parse_bench [ "bench"; "--list"; "-p"; "std" ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" then
        Ok ()
      else
        Error "expected bench --list flag to be parsed"

let test_run_accepts_missing_name = fun _ctx ->
  match parse_run [ "run" ] with
  | Error err -> Error ("expected run args to parse without a name: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:None ~actual:(ArgParser.get_one matches "name");
      Ok ()

let test_run_accepts_list_flag = fun _ctx ->
  match parse_run [ "run"; "--list" ] with
  | Error err -> Error ("expected run args to parse with --list: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" then
        Ok ()
      else
        Error "expected run --list flag to be parsed"

let test_run_accepts_list_json_flag = fun _ctx ->
  match parse_run [ "run"; "--list"; "--json" ] with
  | Error err -> Error ("expected run args to parse with --list --json: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" && ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected run --list --json flags to be parsed"

let test_run_accepts_release_flag = fun _ctx ->
  match parse_run [ "run"; "--release"; "riot" ] with
  | Error err -> Error ("expected run args to parse with --release: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected run --release flag to be parsed"

let test_run_accepts_update_flag = fun _ctx ->
  match parse_run [ "run"; "--update"; "leostera/hello-world" ] with
  | Error err -> Error ("expected run args to parse with --update: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "update" then
        Ok ()
      else
        Error "expected run --update flag to be parsed"

let test_install_accepts_update_flag = fun _ctx ->
  match parse_install [ "install"; "--update"; "leostera/hello-world" ] with
  | Error err -> Error ("expected install args to parse with --update: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "update" then
        Ok ()
      else
        Error "expected install --update flag to be parsed"

let test_install_accepts_missing_name = fun _ctx ->
  match parse_install [ "install" ] with
  | Error err -> Error ("expected install args to parse without a name: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:None ~actual:(ArgParser.get_one matches "name");
      Ok ()

let test_install_accepts_package_flag = fun _ctx ->
  match parse_install [ "install"; "--package"; "riot-cli"; "riot" ] with
  | Error err -> Error ("expected install args to parse with --package: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "riot-cli") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_info_accepts_json_flag = fun _ctx ->
  match parse_info [ "info"; "--json" ] with
  | Error err -> Error ("expected info args to parse with --json: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected info --json flag to be parsed"

let test_run_defaults_remote_binary_to_repo_name = fun _ctx ->
  Test.assert_equal
    ~expected:"hello-world"
    ~actual:(Riot_cli.Run.default_remote_binary_name "leostera/hello-world");
  Test.assert_equal
    ~expected:"hello-world"
    ~actual:(Riot_cli.Run.default_remote_binary_name "github.com/leostera/hello-world/packages/demo");
  Ok ()

let test_run_rejects_trailing_remote_binary_separator = fun _ctx ->
  match Riot_cli.Run.run_with_workspace_info
    ~workspace:None
    ~workspace_error:None (parse_run [ "run"; "leostera/hello-world@" ] |> Result.expect ~msg:"expected run args to parse") with
  | Ok () -> Error "expected trailing @ remote target to fail"
  | Error (Failure message) ->
      if
        String.equal message "invalid remote target 'leostera/hello-world@': expected binary name after @"
      then
        Ok ()
      else
        Error ("unexpected trailing @ error: " ^ message)
  | Error err -> Error ("unexpected error kind: " ^ Kernel.Exception.to_string err)

let make_workspace = fun binaries ->
  let package = Riot_model.Package.make
    ~name:"demo"
    ~path:(Path.v "/workspace/packages/demo")
    ~relative_path:(Path.v "packages/demo")
    ~binaries
    () in
  Riot_model.Workspace.make ~root:(Path.v "/workspace") ~packages:[ package ] ()

let make_workspace_with_packages = fun packages ->
  Riot_model.Workspace.make ~root:(Path.v "/workspace") ~packages ()

let test_run_build_scope_uses_runtime_for_runtime_binaries = fun _ctx ->
  let workspace = make_workspace
    [ Riot_model.Package.{ name = "demo"; path = Path.v "src/demo.ml" } ] in
  Test.assert_equal
    ~expected:Riot_cli.Build.Runtime
    ~actual:(Riot_cli.Run.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"demo");
  Ok ()

let test_run_build_scope_uses_dev_for_test_binaries = fun _ctx ->
  let workspace = make_workspace
    [ Riot_model.Package.{ name = "pm_tests"; path = Path.v "tests/pm_tests.ml" } ] in
  Test.assert_equal
    ~expected:Riot_cli.Build.Dev
    ~actual:(Riot_cli.Run.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"pm_tests");
  Ok ()

let test_run_build_scope_defaults_to_runtime_when_binary_is_missing = fun _ctx ->
  let workspace = make_workspace [] in
  Test.assert_equal
    ~expected:Riot_cli.Build.Runtime
    ~actual:(Riot_cli.Run.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"missing");
  Ok ()

let test_run_resolves_single_implicit_binary = fun _ctx ->
  let workspace = make_workspace
    [ Riot_model.Package.{ name = "hello-world"; path = Path.v "src/hello_world.ml" } ] in
  match Riot_cli.Run.resolve_implicit_local_target workspace with
  | Ok { package_name; binary_name } ->
      Test.assert_equal ~expected:"demo" ~actual:package_name;
      Test.assert_equal ~expected:"hello-world" ~actual:binary_name;
      Ok ()
  | Error err -> Error ("expected single implicit binary to resolve: " ^ err)

let test_run_resolves_single_implicit_binary_in_package = fun _ctx ->
  let demo = Riot_model.Package.make
    ~name:"demo"
    ~path:(Path.v "/workspace/packages/demo")
    ~relative_path:(Path.v "packages/demo")
    ~binaries:[ Riot_model.Package.{ name = "demo"; path = Path.v "src/demo.ml" } ]
    () in
  let util = Riot_model.Package.make
    ~name:"util"
    ~path:(Path.v "/workspace/packages/util")
    ~relative_path:(Path.v "packages/util")
    ~binaries:[ Riot_model.Package.{ name = "util"; path = Path.v "src/util.ml" } ]
    () in
  let workspace = make_workspace_with_packages [ demo; util ] in
  match Riot_cli.Run.resolve_implicit_local_target ~package_filter:"util" workspace with
  | Ok { package_name; binary_name } ->
      Test.assert_equal ~expected:"util" ~actual:package_name;
      Test.assert_equal ~expected:"util" ~actual:binary_name;
      Ok ()
  | Error err -> Error ("expected package-filtered implicit binary to resolve: " ^ err)

let test_run_rejects_ambiguous_implicit_binary = fun _ctx ->
  let demo = Riot_model.Package.make
    ~name:"demo"
    ~path:(Path.v "/workspace/packages/demo")
    ~relative_path:(Path.v "packages/demo")
    ~binaries:[ Riot_model.Package.{ name = "demo"; path = Path.v "src/demo.ml" } ]
    () in
  let util = Riot_model.Package.make
    ~name:"util"
    ~path:(Path.v "/workspace/packages/util")
    ~relative_path:(Path.v "packages/util")
    ~binaries:[ Riot_model.Package.{ name = "util"; path = Path.v "src/util.ml" } ]
    () in
  let workspace = make_workspace_with_packages [ demo; util ] in
  match Riot_cli.Run.resolve_implicit_local_target workspace with
  | Ok _ -> Error "expected implicit run target resolution to reject multiple binaries"
  | Error err ->
      if String.contains err "multiple runnable binaries found" then
        Ok ()
      else
        Error ("expected ambiguity error, got: " ^ err)

let test_run_reports_no_binaries_with_creation_hint = fun _ctx ->
  let workspace = make_workspace [] in
  match Riot_cli.Run.resolve_implicit_local_target workspace with
  | Ok _ -> Error "expected implicit run target resolution to reject missing binaries"
  | Error err ->
      if
        String.equal err "no runnable binaries found; pass a binary name or create one with `riot new --bin ./packages/my-binary`"
      then
        Ok ()
      else
        Error ("expected no-binaries hint, got: " ^ err)

let test_run_reports_package_without_binaries_with_creation_hint = fun _ctx ->
  let workspace = make_workspace [] in
  match Riot_cli.Run.resolve_implicit_local_target ~package_filter:"demo" workspace with
  | Ok _ -> Error "expected package-filtered implicit run target resolution to reject missing binaries"
  | Error err ->
      if
        String.equal err "package 'demo' has no runnable binaries; create one with `riot new --bin ./packages/my-binary`"
      then
        Ok ()
      else
        Error ("expected package no-binaries hint, got: " ^ err)

let test_pm_event_hides_workspace_resolved_packages = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Riot_cli.Build.format_pm_event
    ~seen_registry_updates
    (Riot_model.Event.PackageResolvedForBuild {
      package = "create-riot-app";
      version = None;
      path = "/workspace";
      workspace = true
    }) in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_hides_materialization_started = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Riot_cli.Build.format_pm_event
    ~seen_registry_updates
    (Riot_model.Event.PackageMaterializationStarted {
      package = "std";
      version = "0.1.0";
      path = "/cache/std"
    }) in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_hides_manifest_fetch_chatter = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Riot_cli.Build.format_pm_event
    ~seen_registry_updates
    (Riot_model.Event.PackageManifestFetchStarted { package = "std"; version = "0.1.0" }) in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_hides_download_skipped = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Riot_cli.Build.format_pm_event
    ~seen_registry_updates
    (Riot_model.Event.PackageDownloadSkipped {
      package = "std";
      version = "0.1.0";
      path = "/cache/std";
      reason = "already materialized"
    }) in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_shows_installing_with_padding = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Riot_cli.Build.format_pm_event
    ~seen_registry_updates
    (Riot_model.Event.SourceDependencyMaterializationStarted {
      source_locator = "leostera/hello-world";
      ref_ = None
    }) in
  Test.assert_equal ~expected:(Some "  \027[1;34mInstalling\027[0m leostera/hello-world") ~actual;
  Ok ()

let test_pm_event_shows_locked_package = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Riot_cli.Build.format_pm_event
    ~seen_registry_updates
    (Riot_model.Event.PackageVersionLocked { package = "std"; version = "0.2.0" }) in
  Test.assert_equal ~expected:(Some "      \027[1;32mLocked\027[0m std (0.2.0)") ~actual;
  Ok ()

let test_pm_event_shows_up_to_date = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Riot_cli.Build.format_pm_event
    ~seen_registry_updates
    (Riot_model.Event.PackageVersionsUnchanged { packages = 3 }) in
  Test.assert_equal ~expected:(Some "    Dependencies are already up to date") ~actual;
  Ok ()

let tests =
  Test.[
    case "build: accept multiple package arguments" test_build_accepts_multiple_packages;
    case "build: usage shows variadic packages" test_build_usage_shows_variadic_packages;
    case "build: parse --json flag" test_build_accepts_json_flag;
    case "build: parse --release flag" test_build_accepts_release_flag;
    case "test: parse --json flag" test_test_accepts_json_flag;
    case "test: parse --release flag" test_test_accepts_release_flag;
    case "test: parse --list flag" test_test_accepts_list_flag;
    case "bench: parse --json flag" test_bench_accepts_json_flag;
    case "bench: parse --release flag" test_bench_accepts_release_flag;
    case "bench: parse --list flag" test_bench_accepts_list_flag;
    case "run: parse missing name" test_run_accepts_missing_name;
    case "run: parse --list flag" test_run_accepts_list_flag;
    case "run: parse --list --json flags" test_run_accepts_list_json_flag;
    case "run: parse --release flag" test_run_accepts_release_flag;
    case "run: parse --update flag" test_run_accepts_update_flag;
    case "install: parse missing name" test_install_accepts_missing_name;
    case "install: parse --update flag" test_install_accepts_update_flag;
    case "install: parse --package flag" test_install_accepts_package_flag;
    case "info: parse --json flag" test_info_accepts_json_flag;
    case "run: remote source defaults binary to repo name" test_run_defaults_remote_binary_to_repo_name;
    case "run: trailing @ in remote target is rejected" test_run_rejects_trailing_remote_binary_separator;
    case "run: runtime binaries use runtime scope" test_run_build_scope_uses_runtime_for_runtime_binaries;
    case "run: test binaries use dev scope" test_run_build_scope_uses_dev_for_test_binaries;
    case "run: missing binaries default to runtime scope" test_run_build_scope_defaults_to_runtime_when_binary_is_missing;
    case "run: single implicit binary resolves" test_run_resolves_single_implicit_binary;
    case "run: package-filtered implicit binary resolves" test_run_resolves_single_implicit_binary_in_package;
    case "run: ambiguous implicit binary is rejected" test_run_rejects_ambiguous_implicit_binary;
    case "run: missing binaries suggest creating one" test_run_reports_no_binaries_with_creation_hint;
    case "run: package with no binaries suggests creating one" test_run_reports_package_without_binaries_with_creation_hint;
    case "build: pm events hide workspace resolved packages" test_pm_event_hides_workspace_resolved_packages;
    case "build: pm materialization start is hidden" test_pm_event_hides_materialization_started;
    case "build: pm manifest fetch chatter is hidden" test_pm_event_hides_manifest_fetch_chatter;
    case "build: pm download skipped is hidden" test_pm_event_hides_download_skipped;
    case "build: pm installing source is shown with padding" test_pm_event_shows_installing_with_padding;
    case "build: pm locked package is shown" test_pm_event_shows_locked_package;
    case "build: pm no-op update is shown" test_pm_event_shows_up_to_date;
  ]

let name = "Riot CLI Build Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
