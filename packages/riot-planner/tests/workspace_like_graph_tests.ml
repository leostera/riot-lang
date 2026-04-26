open Std
open Riot_model

module Test = Std.Test
module Package_graph = Riot_planner.Package_graph
module Package = Package
module Workspace = Workspace

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let dependency = fun name ->
  Package.{
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

let make_package = fun
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  name ->
  Package.make
    ~name:(package_name name)
    ~path:(Path.v ("packages/" ^ name))
    ~relative_path:(Path.v ("packages/" ^ name))
    ~dependencies:(List.map dependencies ~fn:dependency)
    ~dev_dependencies:(List.map dev_dependencies ~fn:dependency)
    ~build_dependencies:(List.map build_dependencies ~fn:dependency)
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_workspace = fun packages ->
  Workspace.make_realized
    ~root:(Path.v "/tmp/workspace_like_graph_tests")
    ~packages
    ()

let node_for = fun graph package_name scope ->
  Package_graph.package_key ~package_name scope
  |> Package_graph.get_node_by_key graph
  |> Option.expect ~msg:("missing node: " ^ package_name)

let dependency_keys_for_node = fun graph node ->
  Package_graph.get_dependencies_for_node graph node
  |> List.map ~fn:Package_graph.get_key
  |> List.sort ~compare:Package.key_compare

let package_keys_for_scope = fun scope names ->
  names
  |> List.map ~fn:(fun name -> Package_graph.package_key ~package_name:name scope)
  |> List.sort ~compare:Package.key_compare

let assert_same_keys = fun ~expected ~actual ->
  Test.assert_equal
    ~expected:(List.sort expected ~compare:Package.key_compare)
    ~actual:(List.sort actual ~compare:Package.key_compare)

let runtime_scope_wires_workspace_like_graph = fun _ctx ->
  let packages = [
    make_package "std";
    make_package ~dependencies:[ "std" ] "kernel";
    make_package ~dependencies:[ "std"; "kernel" ] "actors";
    make_package ~dependencies:[ "std"; "actors" ] "riot-model";
    make_package ~dependencies:[ "std"; "riot-model" ] "riot-planner";
    make_package ~dependencies:[ "std"; "riot-model"; "riot-planner" ] "riot-store";
    make_package ~dependencies:[ "std"; "riot-model"; "riot-planner"; "riot-store"; ] "riot-build";
  ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  Test.assert_equal ~expected:(List.length packages) ~actual:(Package_graph.size graph);
  let server_runtime = node_for graph "riot-build" Runtime in
  assert_same_keys
    ~expected:(package_keys_for_scope Runtime [ "std"; "riot-model"; "riot-planner"; "riot-store"; ])
    ~actual:(dependency_keys_for_node graph server_runtime);
  Ok ()

let dev_scope_does_not_inherit_build_only_dependencies = fun _ctx ->
  let packages = [
    make_package "std";
    make_package "codegen";
    make_package "runtime-lib";
    make_package "propane";
    make_package
      ~dependencies:[ "runtime-lib" ]
      ~dev_dependencies:[ "propane" ]
      ~build_dependencies:[ "codegen" ]
      "app";
  ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Dev workspace
    |> Result.expect ~msg:"expected dev graph"
  in
  let app_dev = node_for graph "app" Dev in
  assert_same_keys
    ~expected:(package_keys_for_scope Runtime [ "propane"; "app" ])
    ~actual:(dependency_keys_for_node graph app_dev);
  Ok ()

let missing_workspace_dependencies_are_reported = fun _ctx ->
  let packages = [
    make_package ~dependencies:[ "missing_a" ] "left";
    make_package ~dependencies:[ "missing_b" ] "right";
  ]
  in
  let workspace = make_workspace packages in
  match Package_graph.create ~scope:Runtime workspace with
  | Ok _ -> Error "expected missing dependency error"
  | Error (Package_graph.MissingPackages { missing }) ->
      let missing_pairs =
        List.map
          missing
          ~fn:(fun (item: Package_graph.missing_dependency) -> item.package ^ "->" ^ item.dependency)
        |> List.sort ~compare:String.compare
      in
      Test.assert_equal ~expected:[ "left->missing_a"; "right->missing_b" ] ~actual:missing_pairs;
      Ok ()

let filter_for_package_keeps_only_transitive_dependencies = fun _ctx ->
  let packages = [
    make_package "std";
    make_package ~dependencies:[ "std" ] "kernel";
    make_package ~dependencies:[ "kernel" ] "a";
    make_package ~dependencies:[ "a" ] "app";
    make_package ~dependencies:[ "std" ] "unrelated";
  ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let filtered = Package_graph.filter_for_package graph (package_name "app") in
  let package_names =
    Package_graph.packages filtered
    |> List.map ~fn:(fun (pkg: Package.t) -> pkg.name)
    |> List.unique ~compare:Package_name.compare
  in
  Test.assert_equal
    ~expected:(List.map [ "a"; "app"; "kernel"; "std"; ] ~fn:package_name)
    ~actual:package_names;
  Ok ()

let topological_sort_places_dependencies_before_dependents = fun _ctx ->
  let packages = [
    make_package "std";
    make_package ~dependencies:[ "std" ] "kernel";
    make_package ~dependencies:[ "kernel" ] "actors";
    make_package ~dependencies:[ "actors" ] "app";
  ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let sorted =
    Package_graph.topological_sort graph
    |> List.map ~fn:Package_graph.get_key
  in
  let position_of key =
    List.enumerate sorted
    |> List.find ~fn:(fun (_, current) -> Package.key_equal key current)
    |> Option.map ~fn:(fun (index, _) -> index)
    |> Option.expect ~msg:("missing key in topo sort: " ^ Package.key_to_string key)
  in
  let std_runtime = Package_graph.package_key ~package_name:"std" Runtime in
  let kernel_runtime = Package_graph.package_key ~package_name:"kernel" Runtime in
  let actors_runtime = Package_graph.package_key ~package_name:"actors" Runtime in
  let app_runtime = Package_graph.package_key ~package_name:"app" Runtime in
  Test.assert_true (position_of std_runtime < position_of kernel_runtime);
  Test.assert_true (position_of kernel_runtime < position_of actors_runtime);
  Test.assert_true (position_of actors_runtime < position_of app_runtime);
  Ok ()

let runtime_nodes_with_build_dependencies_depend_on_their_build_nodes = fun _ctx ->
  let packages = [
    make_package "std";
    make_package "codegen";
    make_package ~dependencies:[ "std" ] ~build_dependencies:[ "codegen" ] "app";
  ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let app_runtime = node_for graph "app" Runtime in
  let dependency_keys = dependency_keys_for_node graph app_runtime in
  let app_build_key = Package_graph.package_key ~package_name:"app" Build in
  Test.assert_true (List.any dependency_keys ~fn:(Package.key_equal app_build_key));
  Ok ()

let scope_node_counts_match_expected_projection = fun _ctx ->
  let packages = [
    make_package "std";
    make_package ~dependencies:[ "std" ] "kernel";
    make_package ~dependencies:[ "kernel" ] "app";
  ]
  in
  let workspace = make_workspace packages in
  let build_graph =
    Package_graph.create ~scope:Build workspace
    |> Result.expect ~msg:"expected build graph"
  in
  let runtime_graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let dev_graph =
    Package_graph.create ~scope:Dev workspace
    |> Result.expect ~msg:"expected dev graph"
  in
  Test.assert_equal ~expected:3 ~actual:(Package_graph.size build_graph);
  Test.assert_equal ~expected:3 ~actual:(Package_graph.size runtime_graph);
  Test.assert_equal ~expected:6 ~actual:(Package_graph.size dev_graph);
  Ok ()

let filter_for_unknown_package_returns_empty_graph = fun _ctx ->
  let packages = [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ] in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let filtered = Package_graph.filter_for_package graph (package_name "does-not-exist") in
  Test.assert_equal ~expected:0 ~actual:(Package_graph.size filtered);
  Ok ()

let get_unplanned_dependencies_only_reports_unplanned_runtime_dependencies = fun _ctx ->
  let std = make_package "std" in
  let kernel = make_package ~dependencies:[ "std" ] "kernel" in
  let app = make_package ~dependencies:[ "kernel" ] "app" in
  let workspace = make_workspace [ std; kernel; app ] in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let std_runtime_key = Package_graph.package_key ~package_name:"std" Runtime in
  Package_graph.mark_planned
    graph
    std_runtime_key
    ~module_graph:(Graph.SimpleGraph.make ())
    ~action_graph:(Riot_planner.Action_graph.create ())
    ~hash:(Crypto.hash_string "std-runtime");
  let unplanned = Package_graph.get_unplanned_dependencies graph app in
  Test.assert_equal
    ~expected:(List.map [ "kernel" ] ~fn:package_name)
    ~actual:(List.map unplanned ~fn:(fun (pkg: Package.t) -> pkg.name));
  Ok ()

let build_scope_wires_declared_build_dependencies = fun _ctx ->
  let packages = [ make_package "codegen"; make_package ~build_dependencies:[ "codegen" ] "app" ] in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Build workspace
    |> Result.expect ~msg:"expected build graph"
  in
  let app_build = node_for graph "app" Build in
  let deps = dependency_keys_for_node graph app_build in
  let codegen_build_key = Package_graph.package_key ~package_name:"codegen" Build in
  Test.assert_true (List.any deps ~fn:(Package.key_equal codegen_build_key));
  Ok ()

let tests =
  Test.[
    case "runtime scope wires workspace-like graph" runtime_scope_wires_workspace_like_graph;
    case
      "dev scope does not inherit build-only dependencies"
      dev_scope_does_not_inherit_build_only_dependencies;
    case "missing workspace dependencies are reported" missing_workspace_dependencies_are_reported;
    case
      "filter_for_package keeps only transitive dependencies"
      filter_for_package_keeps_only_transitive_dependencies;
    case
      "topological sort places dependencies before dependents"
      topological_sort_places_dependencies_before_dependents;
    case
      "runtime nodes with build dependencies depend on their own build nodes"
      runtime_nodes_with_build_dependencies_depend_on_their_build_nodes;
    case "scope node counts match expected projection" scope_node_counts_match_expected_projection;
    case
      "filter_for_unknown_package returns empty graph"
      filter_for_unknown_package_returns_empty_graph;
    case
      "get_unplanned_dependencies only reports unplanned runtime dependencies"
      get_unplanned_dependencies_only_reports_unplanned_runtime_dependencies;
    case
      "build scope wires declared build dependencies"
      build_scope_wires_declared_build_dependencies;
  ]

let name = "riot-planner:workspace-like-graph"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
