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

let make_package = fun ~workspace_root name ->
  let package_root = Path.(workspace_root / Path.v name) in
  let publish =
    Riot_model.Package.{
      version = Some version;
      description = Some ("Package " ^ name);
      license = Some "Apache-2.0";
      is_public = Some true
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

let make_plan = fun package ->
  Riot_deps.Publisher.{
    package;
    version;
    locator = "github.com/example/" ^ Riot_model.Package_name.to_string package.name;
    selector = "deadbeef"
  }

let make_prepared = fun package ->
  Riot_deps.Publisher.{
    package;
    version;
    locator = "github.com/example/" ^ Riot_model.Package_name.to_string package.name;
    selector = "deadbeef";
    artifact_path =
      Path.(Path.v "/tmp" / Path.v (Riot_model.Package_name.to_string package.name ^ ".tar.gz"));
  }

let make_published_release = fun package_name ->
  Pkgs_ml.Registry.{
    artifact_sha256 = "deadbeef";
    package_name;
    package_version = Std.Version.to_string version;
    manifest = { key = package_name ^ "/manifest.json"; url = "https://example.com/manifest.json" };
    source_archive = {
      key = package_name ^ "/source.tar.gz";
      url = "https://example.com/source.tar.gz"
    };
    claim = { key = package_name ^ "/claim"; created = true };
    release = { key = package_name ^ "/release"; created = true };
    materialization = { manifest = true; source = true };
  }

let stage_name = fun stage ->
  match stage with
  | `fmt -> "fmt"
  | `fix -> "fix"
  | `build -> "build"
  | `metadata -> "metadata"

let event_name = fun event ->
  match event with
  | Riot_publish.CheckStarted { package; stage; _ } -> "started:"
  ^ Riot_model.Package_name.to_string package
  ^ ":"
  ^ stage_name stage
  | Riot_publish.CheckFinished { package; stage; _ } -> "finished:"
  ^ Riot_model.Package_name.to_string package
  ^ ":"
  ^ stage_name stage
  | Riot_publish.Packing { package; _ } -> "packing:" ^ Riot_model.Package_name.to_string package
  | Riot_publish.DryRunPlanned prepared -> "dry-run:"
  ^ Riot_model.Package_name.to_string prepared.package.name
  | Riot_publish.PackagePublished published -> "published:" ^ published.package_name
  | Riot_publish.SkippedAlreadyPublished { package; _ } -> "skipped-already-published:"
  ^ Riot_model.Package_name.to_string package
  | Riot_publish.SkippedNotPublic { package; _ } -> "skipped-not-public:"
  ^ Riot_model.Package_name.to_string package
  | Riot_publish.Fmt _ -> "fmt-event"
  | Riot_publish.Fix _ -> "fix-event"
  | Riot_publish.Build _ -> "build-event"

let make_deps = fun ~call_log ?(published_version_exists = fun ~registry:_ ~package_name:_ ~version:_ ->
  Ok false) ?(publish_prepared = fun ~registry:_ ~api_token:_ (
  prepared: Riot_deps.Publisher.prepared_publish
) ->
  Ok (make_published_release (Riot_model.Package_name.to_string prepared.package.name))) () ->
  Riot_publish.For_test.{
    resolve_registry = (fun () -> Ok (make_registry ()));
    load_api_token =
      (fun ~registry_name:_ ->
        call_log := "token" :: !call_log;
        Ok "token");
    workspace_publish_order = (fun ~packages -> Ok packages);
    published_version_exists;
    run_fmt_check =
      (fun ~emit:_ ~workspace:_ ~package:_ ->
        call_log := "fmt" :: !call_log;
        Ok ());
    run_fix_check =
      (fun ~emit:_ ~registry:_ ~workspace:_ ~request:_ ~package:_ ->
        call_log := "fix" :: !call_log;
        Ok ());
    run_build_check =
      (fun ~emit:_ ~workspace:_ ~package_name:_ ~profile:_ ->
        call_log := "build" :: !call_log;
        Ok ());
    plan_publish =
      (fun ~registry:_ ~publishing_workspace_packages:_ ~package ->
        call_log := "metadata" :: !call_log;
        Ok (make_plan package));
    prepare_publish_artifact =
      (fun ~target_dir_root:_ plan ->
        call_log := "prepare" :: !call_log;
        Ok (make_prepared plan.package));
    publish_prepared =
      (fun ~registry ~api_token prepared ->
        call_log := "publish" :: !call_log;
        publish_prepared ~registry ~api_token prepared);
  }

let test_dry_run_emits_preflight_events_in_order = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let package = make_package ~workspace_root "demo" in
  let workspace = make_workspace [ package ] in
  let call_log = ref [] in
  let events = ref [] in
  match Riot_publish.For_test.publish_with
    ~on_event:(fun event -> events := event_name event :: !events)
    ~deps:(make_deps ~call_log ())
    ~workspace
    ~request:Riot_publish.{ selection = Package (package_name "demo"); skip_check = false }
    ~mode:DryRun () with
  | Error err -> Error ("expected dry run to succeed, got error: "
  ^ Riot_publish.publish_error_message err)
  | Ok outcomes ->
      let actual_events = List.reverse !events in
      let actual_calls = List.reverse !call_log in
      let expected_events = [
        "started:demo:fmt";
        "finished:demo:fmt";
        "started:demo:fix";
        "finished:demo:fix";
        "started:demo:build";
        "finished:demo:build";
        "started:demo:metadata";
        "finished:demo:metadata";
        "packing:demo";
        "dry-run:demo";
      ]
      in
      if not (actual_calls = [ "fmt"; "fix"; "build"; "metadata"; "prepare" ]) then
        Error ("unexpected call order: " ^ String.concat ", " actual_calls)
      else if not (actual_events = expected_events) then
        Error ("unexpected event order: " ^ String.concat ", " actual_events)
      else
        match outcomes with
        | [ Riot_publish.Planned prepared ] when Riot_model.Package_name.equal
          prepared.package.name
          (package_name "demo") -> Ok ()
        | _ -> Error "expected a single planned publish outcome"

let test_skip_check_skips_fix_stage = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let package = make_package ~workspace_root "demo" in
  let workspace = make_workspace [ package ] in
  let call_log = ref [] in
  let events = ref [] in
  match Riot_publish.For_test.publish_with
    ~on_event:(fun event -> events := event_name event :: !events)
    ~deps:(make_deps ~call_log ())
    ~workspace
    ~request:Riot_publish.{ selection = Package (package_name "demo"); skip_check = true }
    ~mode:DryRun () with
  | Error err -> Error ("expected skip-check dry run to succeed, got error: "
  ^ Riot_publish.publish_error_message err)
  | Ok _ ->
      let actual_events = List.reverse !events in
      let actual_calls = List.reverse !call_log in
      let fix_events =
        List.filter actual_events
          ~fn:(fun name ->
            String.contains name "fix")
      in
      if not (actual_calls = [ "fmt"; "build"; "metadata"; "prepare" ]) then
        Error ("unexpected call order: " ^ String.concat ", " actual_calls)
      else if not (List.is_empty fix_events) then
        Error ("expected skip-check to omit fix events, got: " ^ String.concat ", " fix_events)
      else
        Ok ()

let test_publish_mode_uses_token_and_emits_published = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let package = make_package ~workspace_root "demo" in
  let workspace = make_workspace [ package ] in
  let call_log = ref [] in
  let events = ref [] in
  match Riot_publish.For_test.publish_with
    ~on_event:(fun event -> events := event_name event :: !events)
    ~deps:(make_deps ~call_log ())
    ~workspace
    ~request:Riot_publish.{ selection = Package (package_name "demo"); skip_check = false }
    ~mode:Publish () with
  | Error err -> Error ("expected publish mode to succeed, got error: "
  ^ Riot_publish.publish_error_message err)
  | Ok outcomes ->
      let actual_calls = List.reverse !call_log in
      let actual_events = List.reverse !events in
      if not (actual_calls = [ "token"; "fmt"; "fix"; "build"; "metadata"; "prepare"; "publish" ]) then
        Error ("unexpected call order: " ^ String.concat ", " actual_calls)
      else if not (List.any actual_events ~fn:(String.equal "published:demo")) then
        Error ("expected publish event, got: " ^ String.concat ", " actual_events)
      else
        match outcomes with
        | [ Riot_publish.Published release ] when String.equal release.package_name "demo" -> Ok ()
        | _ -> Error "expected a single published outcome"

let test_already_published_package_is_skipped_before_checks = fun _ctx ->
  let workspace_root = Path.v "/workspace" in
  let package = make_package ~workspace_root "demo" in
  let workspace = make_workspace [ package ] in
  let call_log = ref [] in
  let events = ref [] in
  let deps =
    make_deps
      ~call_log
      ~published_version_exists:(fun ~registry:_ ~package_name:_ ~version:_ -> Ok true)
      ()
  in
  match Riot_publish.For_test.publish_with
    ~on_event:(fun event -> events := event_name event :: !events)
    ~deps
    ~workspace
    ~request:Riot_publish.{ selection = Package (package_name "demo"); skip_check = false }
    ~mode:DryRun () with
  | Error err -> Error ("expected already-published package to be skipped, got error: "
  ^ Riot_publish.publish_error_message err)
  | Ok outcomes ->
      let actual_calls = List.reverse !call_log in
      let actual_events = List.reverse !events in
      if not (actual_calls = []) then
        Error ("expected no preflight calls, got: " ^ String.concat ", " actual_calls)
      else if not (actual_events = [ "skipped-already-published:demo" ]) then
        Error ("unexpected events: " ^ String.concat ", " actual_events)
      else
        match outcomes with
        | [ Riot_publish.Skipped { package; _ } ] when Riot_model.Package_name.equal
          package
          (package_name "demo") -> Ok ()
        | _ -> Error "expected a skipped outcome"

let tests =
  Test.[
    case "publish flow: dry run emits preflight events in order" test_dry_run_emits_preflight_events_in_order;
    case "publish flow: skip-check skips the fix stage" test_skip_check_skips_fix_stage;
    case "publish flow: publish mode uses token and emits published" test_publish_mode_uses_token_and_emits_published;
    case "publish flow: already-published package is skipped before checks" test_already_published_package_is_skipped_before_checks;
  ]

let name = "Riot Publish Flow Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
