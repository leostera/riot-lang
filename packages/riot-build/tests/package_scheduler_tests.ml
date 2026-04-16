open Std

module Test = Std.Test
module Build_context = Riot_build.Internal.Build_context
module Build_work = Riot_build.Internal.Build_work
module Lane_result = Riot_build.Internal.Lane_result
module Package_builder = Riot_build.Internal.Package_builder
module Package_scheduler = Riot_build.Internal.Package_scheduler
module Resolved_build = Riot_build.Internal.Resolved_build
module Package_graph = Riot_planner.Package_graph

let package_name = fun name ->
  Riot_model.Package_name.from_string name |> Result.expect ~msg:("invalid package name: " ^ name)

let package_dependency = fun name ->
  Riot_model.Package.{
    name = package_name name;
    source = {
      workspace = true;
      builtin = false;
      path = None;
      source_locator = None;
      ref_ = None;
      version = None;
    };
  }

let write_workspace_manifest = fun ~root ~members ->
  let members =
    members
    |> List.map ~fn:(fun member -> "  \"" ^ Path.to_string member ^ "\"")
    |> String.concat ",\n"
  in
  let content = "[workspace]\nmembers = [\n" ^ members ^ "\n]\n" in
  Fs.write content Path.(root / Path.v "riot.toml")
  |> Result.expect ~msg:"write workspace riot.toml failed"

let make_package = fun ~root ~name ~source ?(dependencies = []) () ->
  let pkg_dir = Path.(root / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let pkg_name = package_name name in
  Fs.create_dir_all src_dir |> Result.expect ~msg:"create src failed";
  Fs.write source Path.(src_dir / Path.v "lib.ml") |> Result.expect ~msg:"write source failed";
  Fs.write
    ("[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n")
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"write riot.toml failed";
  Riot_model.Package.make
    ~name:pkg_name
    ~path:pkg_dir
    ~relative_path:(Path.v name)
    ~dependencies
    ~library:{ path = Path.v "src/lib.ml" }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let make_workspace = fun ~root ~packages ->
  write_workspace_manifest
    ~root
    ~members:(List.map packages ~fn:(fun (pkg: Riot_model.Package.t) -> pkg.relative_path));
  Riot_model.Workspace.make_realized ~root ~packages ()

let make_request = fun ~workspace ~packages ->
  Riot_build.Request.make
    ~workspace
    ~packages
    ~targets:Riot_model.Target.Host
    ~scope:Riot_build.Request.Runtime
    ~profile:Riot_model.Profile.debug
    ()

let with_prepared_lanes = fun request fn ->
  let context = Build_context.make request
    |> Result.expect ~msg:"expected build context creation to succeed" in
  let resolved = Resolved_build.resolve context request
    |> Result.expect ~msg:"expected build resolution to succeed" in
  let toolchain = Riot_toolchain.init ~config:context.toolchain_config
    |> Result.expect ~msg:"expected toolchain initialization to succeed" in
  let lanes =
    Build_work.prepare_lanes context resolved ~toolchain
    |> Result.expect ~msg:"expected lane preparation to succeed"
  in
  fn lanes

let run_package_scheduler = fun request ->
  with_prepared_lanes request
    (fun lanes ->
      let events = ref [] in
      let summary =
        Package_scheduler.run
          ~parallelism:1
          ~on_event:(fun event -> events := event :: !events)
          lanes
      in
      (summary, List.reverse !events))

let package_key_string = fun key -> Riot_model.Package.key_to_string key

let lane_result = fun summary ->
  match summary.Package_scheduler.lane_results with
  | [ lane_result ] -> lane_result
  | results ->
      panic ("expected exactly one lane result, got " ^ Int.to_string (List.length results))

let result_statuses = fun lane_result ->
  Lane_result.results lane_result
  |> List.map ~fn:(fun (result: Package_builder.build_result) -> result.status)

let status_label = function
  | Package_builder.Built _ -> "built"
  | Package_builder.Cached _ -> "cached"
  | Package_builder.Skipped { reason } -> "skipped(" ^ reason ^ ")"
  | Package_builder.Failed err -> "failed(" ^ Package_builder.package_error_to_string err ^ ")"

let planning_round_signatures = fun events ->
  events
  |> List.filter_map ~fn:(function
    | Package_scheduler.PlanningRoundFinished {
      package_count;
      deferred_count;
      execution_required_count;
      finalized_count;
      _;
    } -> Some (package_count, deferred_count, execution_required_count + finalized_count)
    | _ -> None)

let planning_round_counts = fun events ->
  events
  |> List.filter_map ~fn:(function
    | Package_scheduler.PlanningRoundFinished {
      package_count;
      deferred_count;
      execution_required_count;
      finalized_count;
      cached_count;
      skipped_count;
      failed_count;
      error_count;
      _;
    } ->
        Some (
          package_count,
          deferred_count,
          execution_required_count,
          finalized_count,
          cached_count,
          skipped_count,
          failed_count,
          error_count
        )
    | _ -> None)

let test_package_scheduler_follows_finalized_dependency_frontiers = fun _ctx ->
  match Fs.with_tempdir ~prefix:"riot_package_scheduler_frontiers"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let mid = make_package
        ~root:tmpdir
        ~name:"mid"
        ~dependencies:[ package_dependency "lib" ]
        ~source:"let value = Lib.value\n"
        ()
      in
      let app = make_package
        ~root:tmpdir
        ~name:"app"
        ~dependencies:[ package_dependency "mid" ]
        ~source:"let value = Mid.value\n"
        ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; mid; app ] in
      let summary, events =
        run_package_scheduler (make_request ~workspace ~packages:[ package_name "app" ])
      in
      if summary.errors != [] then
        Error "expected dependency frontier scheduling to avoid internal errors"
      else if summary.had_failure then
        Error "expected dependency frontier scheduling to succeed"
      else
        let actual = planning_round_signatures events in
        let expected = [ (3, 2, 1); (2, 1, 1); (1, 0, 1) ] in
        Test.assert_equal ~expected ~actual;
        Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_succeeds_for_dependency_chain = fun _ctx ->
  match Fs.with_tempdir ~prefix:"riot_package_scheduler_success"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app = make_package
        ~root:tmpdir
        ~name:"app"
        ~dependencies:[ package_dependency "lib" ]
        ~source:"let value = Lib.value\n"
        ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] in
      let summary, _events =
        run_package_scheduler (make_request ~workspace ~packages:[ package_name "app" ])
      in
      if summary.errors != [] then
        Error "expected package scheduler run to avoid internal errors"
      else
        let lane_result = lane_result summary in
        let result_keys =
          Lane_result.results lane_result
          |> List.map ~fn:(fun (result: Package_builder.build_result) ->
            package_key_string result.package_key)
        in
        let expected_keys = [
          Package_graph.package_key ~package_name:"lib" Package_graph.Runtime |> package_key_string;
          Package_graph.package_key ~package_name:"app" Package_graph.Runtime |> package_key_string;
        ] in
        let completed_all =
          result_statuses lane_result
          |> List.all ~fn:(function
            | Package_builder.Built _
            | Package_builder.Cached _ -> true
            | Package_builder.Skipped _
            | Package_builder.Failed _ -> false)
        in
        if not completed_all then
          Error "expected dependency chain packages to complete successfully"
        else if summary.had_failure then
          Error "expected successful package scheduler run to avoid failure summary"
        else (
          Test.assert_equal ~expected:expected_keys ~actual:result_keys;
          Ok ()
        ))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_skips_dependents_after_failed_dependency = fun _ctx ->
  match Fs.with_tempdir ~prefix:"riot_package_scheduler_failure"
    (fun tmpdir ->
      let lib = make_package
        ~root:tmpdir
        ~name:"lib"
        ~source:"let value = Missing_module.value\n"
        ()
      in
      let app = make_package
        ~root:tmpdir
        ~name:"app"
        ~dependencies:[ package_dependency "lib" ]
        ~source:"let value = Lib.value\n"
        ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] in
      let summary, events =
        run_package_scheduler (make_request ~workspace ~packages:[ package_name "app" ])
      in
      if summary.errors != [] then
        Error "expected failing package build to stay in package results, not internal scheduler errors"
      else
        let lane_result = lane_result summary in
        let statuses = result_statuses lane_result in
        let planning_counts = planning_round_counts events in
        let first_round_ok =
          match planning_counts with
          | (2, 1, _, _, _, _, _, _) :: _ -> true
          | _ -> false
        in
        let last_round_ok =
          match List.reverse planning_counts with
          | (_, 0, _, 1, _, 1, _, 0) :: _ -> true
          | _ -> false
        in
        if not first_round_ok then
          Error "expected first planning round to defer the dependent package"
        else if not last_round_ok then
          Error "expected final planning round to skip the dependent package"
        else
          match statuses with
          | [ Package_builder.Failed _; Package_builder.Skipped _ ] ->
              if summary.had_failure && Lane_result.had_partial_failure lane_result then
                Ok ()
              else
                Error "expected failing dependency lane to report partial failure"
          | _ ->
              Error
                ("expected dependency failure to produce failed then skipped package results, got "
                ^ String.concat ", " (List.map statuses ~fn:status_label)))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests =
  let open Test in
  [
    case
      "package scheduler: follows finalized dependency frontiers"
      test_package_scheduler_follows_finalized_dependency_frontiers;
    case
      "package scheduler: succeeds for dependency chain"
      test_package_scheduler_succeeds_for_dependency_chain;
    case
      "package scheduler: skips dependents after failed dependency"
      test_package_scheduler_skips_dependents_after_failed_dependency;
  ]

let name = "Riot Package Scheduler Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
