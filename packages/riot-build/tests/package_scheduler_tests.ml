open Std

module Test = Std.Test
module Build_lane = Riot_build.Internal.Build_lane
module Build_context = Riot_build.Internal.Build_context
module Build_work = Riot_build.Internal.Build_work
module Lane_result = Riot_build.Internal.Lane_result
module Package_builder = Riot_build.Internal.Package_builder
module Package_scheduler = Riot_build.Internal.Package_scheduler
module Resolved_build = Riot_build.Internal.Resolved_build
module Package_graph = Riot_planner.Package_graph

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let package_dependency = fun name ->
  Riot_model.Package.{
    name = package_name name;
    source =
      {
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

let write_toolchain_config = fun ~root ~targets ->
  let target_lines =
    targets
    |> List.map ~fn:(fun target -> "\"" ^ target ^ "\"")
    |> String.concat ", "
  in
  Fs.write
    ("[toolchain]\nversion = \""
    ^ Riot_model.Toolchain_config.default_ocaml_version
    ^ "\"\ntargets = ["
    ^ target_lines
    ^ "]\n")
    Path.(root / Path.v "ocaml-toolchain.toml")
  |> Result.expect ~msg:"write ocaml-toolchain.toml failed"

let make_package = fun ~root ~name ~source ?(dependencies = []) () ->
  let pkg_dir = Path.(root / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let pkg_name = package_name name in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"create src failed";
  Fs.write source Path.(src_dir / Path.v "lib.ml")
  |> Result.expect ~msg:"write source failed";
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

let make_workspace = fun ?toolchain_targets ~root ~packages () ->
  write_workspace_manifest
    ~root
    ~members:(List.map packages ~fn:(fun (pkg: Riot_model.Package.t) -> pkg.relative_path));
  (
    match toolchain_targets with
    | Some targets -> write_toolchain_config ~root ~targets
    | None -> ()
  );
  Riot_model.Workspace.make_realized ~root ~packages ()

let make_request = fun
  ~workspace ~packages ?(targets = Riot_model.Target.Host) ?(requested_parallelism = None) () ->
  Riot_build.Request.make
    ~workspace
    ~packages
    ~targets
    ~scope:Riot_build.Request.Runtime
    ~profile:Riot_model.Profile.debug
    ~requested_parallelism
    ()

let with_prepared_lanes = fun request fn ->
  let context =
    Build_context.make request
    |> Result.expect ~msg:"expected build context creation to succeed"
  in
  let resolved =
    Resolved_build.resolve context request
    |> Result.expect ~msg:"expected build resolution to succeed"
  in
  let toolchain =
    Riot_toolchain.init ~config:context.toolchain_config
    |> Result.expect ~msg:"expected toolchain initialization to succeed"
  in
  let lanes =
    Build_work.prepare_lanes context resolved ~toolchain
    |> Result.expect ~msg:"expected lane preparation to succeed"
  in
  try
    let result = fn lanes in
    Build_work.release_lanes lanes;
    result
  with
  | exn ->
      Build_work.release_lanes lanes;
      raise exn

let run_package_scheduler = fun request ->
  with_prepared_lanes
    request
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

let status_label = fun __tmp1 ->
  match __tmp1 with
  | Package_builder.Built _ -> "built"
  | Package_builder.Cached _ -> "cached"
  | Package_builder.Skipped { reason } -> "skipped(" ^ reason ^ ")"
  | Package_builder.Failed err -> "failed(" ^ Package_builder.package_error_to_string err ^ ")"

let planning_phase_counts = fun events ->
  events
  |> List.filter_map
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Package_scheduler.PlanningFinished {
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

let execution_phase_counts = fun events ->
  events
  |> List.filter_map
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Package_scheduler.ExecutionFinished {
          package_count;
          finalized_count;
          built_count;
          failed_count;
          error_count;
          _;
        } ->
          Some (package_count, finalized_count, built_count, failed_count, error_count)
      | _ -> None)

let lane_targets = fun summary ->
  summary.Package_scheduler.lane_results
  |> List.map ~fn:Lane_result.target
  |> List.sort ~compare:Riot_model.Target.compare

let test_package_scheduler_follows_finalized_dependency_frontiers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_frontiers"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let mid =
        make_package
          ~root:tmpdir
          ~name:"mid"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "mid" ]
          ~source:"let value = Mid.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; mid; app ] () in
      let (summary, events) =
        run_package_scheduler (make_request ~workspace ~packages:[ package_name "app" ] ())
      in
      if summary.errors != [] then
        Error "expected dependency frontier scheduling to avoid internal errors"
      else if summary.had_failure then
        Error "expected dependency frontier scheduling to succeed"
      else
        let planning_counts = planning_phase_counts events in
        let execution_counts = execution_phase_counts events in
        Test.assert_equal
          ~expected:[
            (3, 2, 3, 0, 0, 0, 0, 0);
          ]
          ~actual:planning_counts;
      Test.assert_equal
        ~expected:[
          (3, 3, 3, 0, 0);
        ]
        ~actual:execution_counts;
      Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_succeeds_for_dependency_chain = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_success"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] () in
      let (summary, _events) =
        run_package_scheduler (make_request ~workspace ~packages:[ package_name "app" ] ())
      in
      if summary.errors != [] then
        Error "expected package scheduler run to avoid internal errors"
      else
        let lane_result = lane_result summary in
        let result_keys =
          Lane_result.results lane_result
          |> List.map
            ~fn:(fun (result: Package_builder.build_result) -> package_key_string result.package_key)
        in
        let expected_keys = [
          Package_graph.package_key ~package_name:"lib" Package_graph.Runtime
          |> package_key_string;
          Package_graph.package_key ~package_name:"app" Package_graph.Runtime
          |> package_key_string;
        ]
        in
        let completed_all =
          result_statuses lane_result
          |> List.all
            ~fn:(fun __tmp1 ->
              match __tmp1 with
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
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_skips_dependents_after_failed_dependency = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_failure"
    (fun tmpdir ->
      let lib =
        make_package ~root:tmpdir ~name:"lib" ~source:"let value : int = \"not an int\"\n" ()
      in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] () in
      let (summary, events) =
        run_package_scheduler (make_request ~workspace ~packages:[ package_name "app" ] ())
      in
      if summary.errors != [] then
        Error "expected failing package build to stay in package results, not internal scheduler errors"
      else
        let lane_result = lane_result summary in
        let statuses = result_statuses lane_result in
        let planning_counts = planning_phase_counts events in
        let execution_counts = execution_phase_counts events in
        if planning_counts != [
          (2, 1, 1, 1, 0, 1, 0, 0);
        ] then
          Error "expected unified planning summary to report one skipped dependent and one executing dependency"
        else if execution_counts != [
          (1, 1, 0, 1, 0);
        ] then
          Error "expected unified execution summary to report one failed executed package"
        else
          match statuses with
          | [ Package_builder.Failed _; Package_builder.Skipped _ ] ->
              if summary.had_failure && Lane_result.had_partial_failure lane_result then
                Ok ()
              else
                Error "expected failing dependency lane to report partial failure"
          | _ ->
              Error ("expected dependency failure to produce failed then skipped package results, got "
              ^ String.concat ", " (List.map statuses ~fn:status_label))) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_phase_events_have_consistent_counts = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_event_counts"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] () in
      let (summary, events) =
        run_package_scheduler (make_request ~workspace ~packages:[ package_name "app" ] ())
      in
      if summary.errors != [] then
        Error "expected event count validation run to avoid internal errors"
      else
        let planning_counts = planning_phase_counts events in
        let execution_counts = execution_phase_counts events in
        let planning_ok =
          planning_counts
          |> List.all
            ~fn:(fun
              (
                package_count,
                deferred_count,
                execution_required_count,
                finalized_count,
                cached_count,
                skipped_count,
                failed_count,
                error_count
              ) ->
              package_count = execution_required_count + finalized_count + error_count
              && deferred_count <= package_count
              && cached_count <= finalized_count
              && skipped_count <= finalized_count
              && failed_count <= finalized_count)
        in
        let execution_ok =
          execution_counts
          |> List.all
            ~fn:(fun (package_count, finalized_count, built_count, failed_count, error_count) ->
              package_count = finalized_count + error_count
              && built_count <= finalized_count
              && failed_count <= finalized_count)
        in
        if not planning_ok then
          Error "expected planning phase counts to account for every scheduled package"
        else if not execution_ok then
          Error "expected execution phase counts to account for every scheduled package"
        else
          Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_keeps_multi_lane_results_isolated = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_multi_lane"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let workspace =
        make_workspace
          ~root:tmpdir
          ~toolchain_targets:(
            Riot_model.Target.Set.to_list requested_targets
            |> List.map ~fn:Riot_model.Target.to_string
          )
          ~packages:[ make_package ~root:tmpdir ~name:"demo" ~source:"let value = 42\n" () ]
          ()
      in
      let (summary, events) =
        run_package_scheduler
          (make_request
            ~workspace
            ~packages:[ package_name "demo" ]
            ~targets:(Riot_model.Target.Exact requested_targets)
            ~requested_parallelism:(Some 1)
            ())
      in
      if summary.errors != [] then
        Error "expected multi-lane package scheduler run to avoid internal errors"
      else if List.length summary.Package_scheduler.lane_results != 2 then
        Error "expected one lane result per requested target"
      else if List.length summary.Package_scheduler.completions != 2 then
        Error "expected one completion per requested target"
      else
        let actual_targets = lane_targets summary in
        let expected_targets =
          Riot_model.Target.Set.to_list requested_targets
          |> List.sort ~compare:Riot_model.Target.compare
        in
        let all_result_counts_are_one =
          summary.Package_scheduler.completions
          |> List.all
            ~fn:(fun (completion: Package_scheduler.completion) -> completion.result_count = 1)
        in
        let all_lane_results_are_singletons =
          summary.Package_scheduler.lane_results
          |> List.all ~fn:(fun lane_result -> List.length (Lane_result.results lane_result) = 1)
        in
        let lane_count_events_ok =
          events
          |> List.filter_map
            ~fn:(fun
              (Package_scheduler.PlanningStarted { lane_count; _ }
              | Package_scheduler.PlanningFinished { lane_count; _ }
              | Package_scheduler.ExecutionStarted { lane_count; _ }
              | Package_scheduler.ExecutionFinished { lane_count; _ }) ->
              Some lane_count)
          |> List.all ~fn:(fun lane_count -> lane_count = 2)
        in
        if actual_targets != expected_targets then
          Error "expected multi-lane scheduler results to preserve requested targets"
        else if not all_result_counts_are_one then
          Error "expected each multi-lane completion to report one package result"
        else if not all_lane_results_are_singletons then
          Error "expected each multi-lane lane result to stay isolated to one package"
        else if not lane_count_events_ok then
          Error "expected multi-lane events to report both lanes"
        else
          Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_rerun_uses_cached_dependency_frontiers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_cached_rerun"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] () in
      let request = make_request ~workspace ~packages:[ package_name "app" ] () in
      let (first_summary, _first_events) = run_package_scheduler request in
      let (second_summary, second_events) = run_package_scheduler request in
      if first_summary.errors != [] then
        Error "expected initial build to avoid internal scheduler errors"
      else if second_summary.errors != [] then
        Error "expected cached rerun to avoid internal scheduler errors"
      else if second_summary.had_failure then
        Error "expected cached rerun to finish without failures"
      else
        let statuses = result_statuses (lane_result second_summary) in
        let planning = planning_phase_counts second_events in
        let execution_rounds = execution_phase_counts second_events in
        let all_cached =
          List.all
            statuses
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Package_builder.Cached _ -> true
              | Package_builder.Built _
              | Package_builder.Skipped _
              | Package_builder.Failed _ -> false)
        in
        if not all_cached then
          Error "expected cached rerun to finalize every package from cache"
        else if execution_rounds != [] then
          Error "expected cached rerun to skip package execution rounds"
        else (
          Test.assert_equal
            ~expected:[
              (2, 1, 0, 2, 2, 0, 0, 0);
            ]
            ~actual:planning;
          Ok ()
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_cached_rerun_preserves_multi_lane_isolation = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_cached_multi_lane"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let demo = make_package ~root:tmpdir ~name:"demo" ~source:"let value = 42\n" () in
      let workspace =
        make_workspace
          ~root:tmpdir
          ~toolchain_targets:(
            Riot_model.Target.Set.to_list requested_targets
            |> List.map ~fn:Riot_model.Target.to_string
          )
          ~packages:[ demo ]
          ()
      in
      let request =
        make_request
          ~workspace
          ~packages:[ package_name "demo" ]
          ~targets:(Riot_model.Target.Exact requested_targets)
          ~requested_parallelism:(Some 1)
          ()
      in
      let (first_summary, _first_events) = run_package_scheduler request in
      let (second_summary, second_events) = run_package_scheduler request in
      if first_summary.errors != [] then
        Error "expected initial multi-lane build to avoid internal scheduler errors"
      else if second_summary.errors != [] then
        Error "expected cached multi-lane rerun to avoid internal scheduler errors"
      else if second_summary.had_failure then
        Error "expected cached multi-lane rerun to finish without failures"
      else if List.length second_summary.Package_scheduler.lane_results != 2 then
        Error "expected cached multi-lane rerun to keep one result per lane"
      else
        let actual_targets = lane_targets second_summary in
        let expected_targets =
          Riot_model.Target.Set.to_list requested_targets
          |> List.sort ~compare:Riot_model.Target.compare
        in
        let execution_rounds = execution_phase_counts second_events in
        let all_cached =
          second_summary.Package_scheduler.lane_results
          |> List.all
            ~fn:(fun lane_result ->
              result_statuses lane_result
              |> List.all
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | Package_builder.Cached _ -> true
                  | Package_builder.Built _
                  | Package_builder.Skipped _
                  | Package_builder.Failed _ -> false))
        in
        let all_result_counts_are_one =
          second_summary.Package_scheduler.completions
          |> List.all
            ~fn:(fun (completion: Package_scheduler.completion) -> completion.result_count = 1)
        in
        if actual_targets != expected_targets then
          Error "expected cached multi-lane rerun to preserve requested targets"
        else if not all_cached then
          Error "expected cached multi-lane rerun to finalize every lane from cache"
        else if execution_rounds != [] then
          Error "expected cached multi-lane rerun to skip execution rounds"
        else if not all_result_counts_are_one then
          Error "expected cached multi-lane rerun completions to stay per-lane"
        else
          Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_package_scheduler_reports_stalled_pending_work = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_package_scheduler_stall"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] () in
      let request = make_request ~workspace ~packages:[ package_name "app" ] () in
      with_prepared_lanes
        request
        (fun lanes ->
          match lanes with
          | [ lane ] ->
              let package_graph = Build_lane.package_graph lane in
              let lib_key = Package_graph.package_key ~package_name:"lib" Package_graph.Runtime in
              let app_key = Package_graph.package_key ~package_name:"app" Package_graph.Runtime in
              let lib_node =
                Package_graph.get_node_by_key package_graph lib_key
                |> Option.expect ~msg:"expected lib node in prepared package graph"
              in
              let app_node =
                Package_graph.get_node_by_key package_graph app_key
                |> Option.expect ~msg:"expected app node in prepared package graph"
              in
              Graph.SimpleGraph.add_edge lib_node ~depends_on:app_node;
              let events = ref [] in
              let summary =
                Package_scheduler.run
                  ~parallelism:1
                  ~on_event:(fun event -> events := event :: !events)
                  lanes
              in
              let events = List.reverse !events in
              let planning_counts = planning_phase_counts events in
              let execution_rounds = execution_phase_counts events in
              let stalled_errors =
                summary.Package_scheduler.errors
                |> List.all
                  ~fn:(fun (error: Package_scheduler.error) ->
                    String.contains error.reason "made no progress"
                    && String.contains error.reason "awaiting plan")
              in
              let completion_ok =
                match summary.Package_scheduler.completions with
                | [ completion ] -> completion.result_count = 0 && completion.had_partial_failure
                | _ -> false
              in
              if not summary.Package_scheduler.had_failure then
                Error "expected stalled package scheduler run to report failure"
              else if summary.Package_scheduler.errors = [] then
                Error "expected stalled package scheduler run to report internal errors"
              else if summary.Package_scheduler.lane_results != [] then
                Error "expected stalled package scheduler run to produce no finalized lane results"
              else if not stalled_errors then
                Error "expected stalled package scheduler errors to describe pending work"
              else if not completion_ok then
                Error "expected stalled package scheduler completion to report partial failure with zero results"
              else if execution_rounds != [] then
                Error "expected stalled package scheduler run to avoid execution rounds"
              else (
                Test.assert_equal
                  ~expected:[
                    (2, 2, 0, 0, 0, 0, 0, 0);
                  ]
                  ~actual:planning_counts;
                Ok ()
              )
          | _ -> Error "expected single-lane scheduler test setup")) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests = let open Test in
[
  case
    ~size:Large
    "package scheduler: follows finalized dependency frontiers"
    test_package_scheduler_follows_finalized_dependency_frontiers;
  case
    ~size:Large
    "package scheduler: succeeds for dependency chain"
    test_package_scheduler_succeeds_for_dependency_chain;
  case
    "package scheduler: skips dependents after failed dependency"
    test_package_scheduler_skips_dependents_after_failed_dependency;
  case
    ~size:Large
    "package scheduler: phase events have consistent counts"
    test_package_scheduler_phase_events_have_consistent_counts;
  case
    ~size:Large
    "package scheduler: keeps multi-lane results isolated"
    test_package_scheduler_keeps_multi_lane_results_isolated;
  case
    ~size:Large
    "package scheduler: rerun uses cached dependency frontiers"
    test_package_scheduler_rerun_uses_cached_dependency_frontiers;
  case
    ~size:Large
    "package scheduler: cached rerun preserves multi-lane isolation"
    test_package_scheduler_cached_rerun_preserves_multi_lane_isolation;
  case
    "package scheduler: reports stalled pending work"
    test_package_scheduler_reports_stalled_pending_work;
]

let name = "Riot Package Scheduler Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
