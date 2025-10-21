open Std
module Test = Std.Test

let make_package ~name ~deps =
  Tusk_model.Package.
    {
      name;
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies =
        List.map (fun dep_name -> { name = dep_name; source = Workspace }) deps;
      binaries = [];
      library = None;
      test_library = None;
      test_modules = [];
    }

let make_workspace packages =
  Tusk_model.Workspace.
    { root = Path.v "."; target_dir_root = Path.v "target"; packages }

let test_create_empty_workspace () =
  let workspace = make_workspace [] in
  let graph = Tusk_planner.Package_graph.create workspace in
  if Tusk_planner.Package_graph.size graph = 0 then Ok ()
  else Error "Expected empty graph"

let test_create_single_package () =
  let pkg = make_package ~name:"foo" ~deps:[] in
  let workspace = make_workspace [ pkg ] in
  let graph = Tusk_planner.Package_graph.create workspace in
  if Tusk_planner.Package_graph.size graph = 1 then Ok ()
  else Error "Expected 1 package in graph"

let test_topological_sort_no_deps () =
  let pkg_a = make_package ~name:"a" ~deps:[] in
  let pkg_b = make_package ~name:"b" ~deps:[] in
  let workspace = make_workspace [ pkg_a; pkg_b ] in
  let graph = Tusk_planner.Package_graph.create workspace in
  let sorted = Tusk_planner.Package_graph.topological_sort graph in
  if List.length sorted = 2 then Ok () else Error "Expected 2 packages"

let test_topological_sort_with_deps () =
  let pkg_a = make_package ~name:"a" ~deps:[] in
  let pkg_b = make_package ~name:"b" ~deps:[ "a" ] in
  let workspace = make_workspace [ pkg_b; pkg_a ] in
  let graph = Tusk_planner.Package_graph.create workspace in
  let sorted = Tusk_planner.Package_graph.topological_sort graph in
  match sorted with
  | [ first; second ] ->
      if String.equal first.name "a" && String.equal second.name "b" then Ok ()
      else
        Error
          (format "Expected a before b, got %s before %s" first.name second.name)
  | _ -> Error "Expected 2 packages in order"

let test_cycle_detection () =
  let pkg_a = make_package ~name:"a" ~deps:[ "b" ] in
  let pkg_b = make_package ~name:"b" ~deps:[ "a" ] in
  let workspace = make_workspace [ pkg_a; pkg_b ] in
  let graph = Tusk_planner.Package_graph.create workspace in
  match Tusk_planner.Package_graph.topological_sort graph with
  | _ -> Error "Expected cycle detection to raise exception"
  | exception Tusk_planner.Package_graph.Cycle_detected cycle ->
      if List.length cycle > 0 then Ok () else Error "Expected non-empty cycle"

let test_filter_for_package () =
  let pkg_a = make_package ~name:"a" ~deps:[] in
  let pkg_b = make_package ~name:"b" ~deps:[ "a" ] in
  let pkg_c = make_package ~name:"c" ~deps:[ "b" ] in
  let pkg_d = make_package ~name:"d" ~deps:[] in
  let workspace = make_workspace [ pkg_a; pkg_b; pkg_c; pkg_d ] in
  let graph = Tusk_planner.Package_graph.create workspace in
  let filtered = Tusk_planner.Package_graph.filter_for_package graph "c" in
  let size = Tusk_planner.Package_graph.size filtered in
  if size = 3 then Ok ()
  else Error (format "Expected 3 packages (c, b, a), got %d" size)

let test_filter_missing_package () =
  let pkg_a = make_package ~name:"a" ~deps:[] in
  let workspace = make_workspace [ pkg_a ] in
  let graph = Tusk_planner.Package_graph.create workspace in
  let filtered =
    Tusk_planner.Package_graph.filter_for_package graph "missing"
  in
  if Tusk_planner.Package_graph.size filtered = 0 then Ok ()
  else Error "Expected empty graph for missing package"

let test_workspace_planner_all () =
  let pkg_a = make_package ~name:"a" ~deps:[] in
  let pkg_b = make_package ~name:"b" ~deps:[ "a" ] in
  let workspace = make_workspace [ pkg_b; pkg_a ] in
  match
    Tusk_planner.Workspace_planner.plan_workspace ~workspace ~target:All
  with
  | Ok plan ->
      let packages = Tusk_planner.Workspace_planner.packages_in_plan plan in
      if List.length packages = 2 then Ok ()
      else Error (format "Expected 2 packages, got %d" (List.length packages))
  | Error _ -> Error "Planning failed"

let test_workspace_planner_specific_package () =
  let pkg_a = make_package ~name:"a" ~deps:[] in
  let pkg_b = make_package ~name:"b" ~deps:[ "a" ] in
  let pkg_c = make_package ~name:"c" ~deps:[] in
  let workspace = make_workspace [ pkg_a; pkg_b; pkg_c ] in
  match
    Tusk_planner.Workspace_planner.plan_workspace ~workspace
      ~target:(Package "b")
  with
  | Ok plan ->
      let packages = Tusk_planner.Workspace_planner.packages_in_plan plan in
      if List.length packages = 2 then Ok ()
      else
        Error
          (format "Expected 2 packages (b + a), got %d" (List.length packages))
  | Error _ -> Error "Planning failed"

let test_workspace_planner_package_not_found () =
  let pkg_a = make_package ~name:"a" ~deps:[] in
  let workspace = make_workspace [ pkg_a ] in
  match
    Tusk_planner.Workspace_planner.plan_workspace ~workspace
      ~target:(Package "missing")
  with
  | Ok _ -> Error "Expected PackageNotFound error"
  | Error (PackageNotFound { name; available }) ->
      if String.equal name "missing" && List.length available = 1 then Ok ()
      else Error "PackageNotFound error has wrong details"
  | Error _ -> Error "Expected PackageNotFound error"

let test_workspace_planner_cycle_detection () =
  let pkg_a = make_package ~name:"a" ~deps:[ "b" ] in
  let pkg_b = make_package ~name:"b" ~deps:[ "a" ] in
  let workspace = make_workspace [ pkg_a; pkg_b ] in
  match
    Tusk_planner.Workspace_planner.plan_workspace ~workspace ~target:All
  with
  | Ok _ -> Error "Expected CycleDetected error"
  | Error (CycleDetected { cycle }) ->
      if List.length cycle > 0 then Ok () else Error "Expected non-empty cycle"
  | Error _ -> Error "Expected CycleDetected error"

let tests =
  let open Test in
  [
    case "create empty workspace" test_create_empty_workspace;
    case "create single package" test_create_single_package;
    case "topological sort no deps" test_topological_sort_no_deps;
    case "topological sort with deps" test_topological_sort_with_deps;
    case "cycle detection" test_cycle_detection;
    case "filter for package" test_filter_for_package;
    case "filter missing package" test_filter_missing_package;
    case "workspace planner: all" test_workspace_planner_all;
    case "workspace planner: specific package"
      test_workspace_planner_specific_package;
    case "workspace planner: package not found"
      test_workspace_planner_package_not_found;
    case "workspace planner: cycle detection"
      test_workspace_planner_cycle_detection;
  ]

let name = "Workspace Planning Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
