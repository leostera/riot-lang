open Std
open Riot_model

module Test = Std.Test
module Build_unit = Riot_planner.Build_unit
module Build_unit_graph = Riot_planner.Build_unit_graph
module G = Graph.SimpleGraph
module Package = Package
module Workspace = Workspace

let test_root = Path.v "/tmp/workspace_like_graph_tests"

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
  ?(binaries = [])
  ?(sources = None)
  name ->
  Package.make
    ~name:(package_name name)
    ~path:Path.(test_root / Path.v ("packages/" ^ name))
    ~relative_path:(Path.v ("packages/" ^ name))
    ~dependencies:(List.map dependencies ~fn:dependency)
    ~dev_dependencies:(List.map dev_dependencies ~fn:dependency)
    ~build_dependencies:(List.map build_dependencies ~fn:dependency)
    ~binaries
    ~library:{ path = Path.v "src/lib.ml" }
    ?sources
    ()

let make_workspace = fun packages ->
  Workspace.make_realized
    ~root:test_root
    ~packages
    ()

let test_toolchain =
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"expected test toolchain"

let host_target = Target.host ()

let default_dev_artifacts = Package.{ tests = true; examples = true; benches = true }

let request = fun ?roots ?(kind = Build_unit_graph.Runtime) ?(synthetic_tools = []) () ->
  Build_unit_graph.{
    roots;
    targets = [ host_target ];
    profile = Profile.debug;
    kind;
    synthetic_tools;
  }

let graph = fun workspace request ->
  Build_unit_graph.create workspace request
  |> Result.expect ~msg:"expected build unit graph"

let library_key = fun package ->
  ({
    package = package_name package;
    artifact = Build_unit.Library;
    target = host_target;
    profile = Profile.debug;
  }:Build_unit.key)

let synthetic_key = fun package ->
  ({
    package = package_name package;
    artifact = Build_unit.SyntheticTool { name = "build" };
    target = host_target;
    profile = Profile.debug;
  }:Build_unit.key)

let test_binary_key = fun package name ->
  ({
    package = package_name package;
    artifact = Build_unit.TestBinary { name };
    target = host_target;
    profile = Profile.debug;
  }:Build_unit.key)

let dependency_keys_for_key = fun graph key -> Build_unit_graph.dependencies graph key

let library_keys = fun names ->
  names
  |> List.map ~fn:library_key
  |> List.sort ~compare:Build_unit.compare_key

let assert_same_keys = fun ~expected ~actual ->
  let expected =
    List.map (List.sort expected ~compare:Build_unit.compare_key) ~fn:Build_unit.key_to_string
  in
  let actual =
    List.map (List.sort actual ~compare:Build_unit.compare_key) ~fn:Build_unit.key_to_string
  in
  if expected != actual then
    panic
      ("expected keys:\n"
      ^ String.concat "\n" expected
      ^ "\nactual keys:\n"
      ^ String.concat "\n" actual)

let assert_int_equal = fun ~label ~expected ~actual ->
  if not (Int.equal expected actual) then
    panic
      (label
      ^ ": expected "
      ^ Int.to_string expected
      ^ ", actual "
      ^ Int.to_string actual)

let package_names_in_graph = fun graph ->
  Build_unit_graph.keys graph
  |> List.map ~fn:(fun (key: Build_unit.key) -> key.package)
  |> List.unique ~compare:Package_name.compare
  |> List.sort ~compare:Package_name.compare

let build_unit_for_key = fun graph key ->
  Build_unit_graph.find graph key
  |> Option.map ~fn:Build_unit_graph.node_value
  |> Option.expect ~msg:("missing build unit: " ^ Build_unit.key_to_string key)

let module_for_package = fun package path ->
  Riot_model.Module.make
    ~namespace:(Namespace.from_list [ Package.root_module_name package ])
    ~filename:(Path.v path)

let module_node_for_path = fun module_graph path ->
  let found = ref None in
  Graph.SimpleGraph.iter
    module_graph
    ~fn:(fun _id (node: Riot_planner.Module_node.t G.node) ->
      let node_value = G.value node in
      match node_value.file with
      | Concrete node_path when Path.equal node_path (Path.v path) -> found := Some node
      | Concrete _
      | Generated _ -> ());
  !found
  |> Option.expect ~msg:("expected module graph node for " ^ path)

let add_write_action = fun action_graph package path content ->
  let spec =
    Riot_planner.Action_node.make
      ~actions:[ Riot_planner.Action.WriteFile { destination = Path.v path; content } ]
      ~outs:[ Path.v path ]
      ~srcs:[]
      ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  Riot_planner.Action_graph.add_node action_graph spec

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
  let graph = graph workspace (request ()) in
  Test.assert_equal ~expected:(List.length packages) ~actual:(Build_unit_graph.size graph);
  assert_same_keys
    ~expected:(library_keys [ "std"; "riot-model"; "riot-planner"; "riot-store"; ])
    ~actual:(dependency_keys_for_key graph (library_key "riot-build"));
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
      ~binaries:[ Package.{ name = "app_tests"; path = Path.v "tests/app_tests.ml" } ]
      ~sources:(Some {
        src = [ Path.v "src/lib.ml" ];
        native = [];
        tests = [ Path.v "tests/app_tests.ml" ];
        examples = [];
        bench = [];
      })
      "app";
  ]
  in
  let workspace = make_workspace packages in
  let graph =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~kind:(Build_unit_graph.Dev default_dev_artifacts)
        ())
  in
  assert_same_keys
    ~expected:((library_key "app") :: library_keys [ "runtime-lib"; "propane" ])
    ~actual:(dependency_keys_for_key graph (test_binary_key "app" "app_tests"));
  Test.assert_false
    (List.contains
      (dependency_keys_for_key graph (test_binary_key "app" "app_tests"))
      ~value:(library_key "codegen"));
  Ok ()

let missing_workspace_dependencies_are_reported = fun _ctx ->
  let packages = [
    make_package ~dependencies:[ "missing_a" ] "left";
    make_package ~dependencies:[ "missing_b" ] "right";
  ]
  in
  let workspace = make_workspace packages in
  match Build_unit_graph.create workspace (request ()) with
  | Ok _ -> Error "expected missing dependency error"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      let missing_pairs =
        List.map
          missing
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Build_unit_graph.Dependency { package; dependency } ->
                Package_name.to_string package ^ "->" ^ Package_name.to_string dependency
            | Root package -> "root:" ^ Package_name.to_string package)
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
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  Test.assert_equal
    ~expected:(List.map [ "a"; "app"; "kernel"; "std"; ] ~fn:package_name)
    ~actual:(package_names_in_graph graph);
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
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  let sorted =
    Build_unit_graph.topological_sort graph
    |> Result.expect ~msg:"expected acyclic build unit graph"
    |> List.map ~fn:Build_unit.key
  in
  let position_of key =
    List.enumerate sorted
    |> List.find ~fn:(fun (_, current) -> Build_unit.equal_key key current)
    |> Option.map ~fn:(fun (index, _) -> index)
    |> Option.expect ~msg:("missing key in topo sort: " ^ Build_unit.key_to_string key)
  in
  Test.assert_true (position_of (library_key "std") < position_of (library_key "kernel"));
  Test.assert_true (position_of (library_key "kernel") < position_of (library_key "actors"));
  Test.assert_true (position_of (library_key "actors") < position_of (library_key "app"));
  Ok ()

let runtime_nodes_with_build_dependencies_depend_on_their_build_nodes = fun _ctx ->
  let packages = [
    make_package "std";
    make_package "codegen";
    make_package ~dependencies:[ "std" ] ~build_dependencies:[ "codegen" ] "app";
  ]
  in
  let workspace = make_workspace packages in
  let runtime_graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_same_keys
    ~expected:(library_keys [ "std" ])
    ~actual:(dependency_keys_for_key runtime_graph (library_key "app"));
  let build_graph =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~synthetic_tools:[ Build_unit_graph.{ package = package_name "app"; name = "build" } ]
        ())
  in
  assert_same_keys
    ~expected:(library_keys [ "app"; "codegen" ])
    ~actual:(dependency_keys_for_key build_graph (synthetic_key "app"));
  Ok ()

let build_unit_requests_select_expected_artifacts = fun _ctx ->
  let packages = [
    make_package "std";
    make_package ~dependencies:[ "std" ] "kernel";
    make_package ~dependencies:[ "kernel" ] "app";
  ]
  in
  let workspace = make_workspace packages in
  let runtime_graph = graph workspace (request ()) in
  let dev_graph =
    graph workspace (request ~kind:(Build_unit_graph.Dev default_dev_artifacts) ())
  in
  let build_graph =
    graph
      workspace
      (request
        ~synthetic_tools:[
          Build_unit_graph.{ package = package_name "std"; name = "build" };
          { package = package_name "kernel"; name = "build" };
          { package = package_name "app"; name = "build" };
        ]
        ())
  in
  assert_int_equal ~label:"runtime graph size" ~expected:3 ~actual:(Build_unit_graph.size runtime_graph);
  assert_int_equal ~label:"dev graph size" ~expected:3 ~actual:(Build_unit_graph.size dev_graph);
  assert_int_equal ~label:"build graph size" ~expected:6 ~actual:(Build_unit_graph.size build_graph);
  Ok ()

let dev_filter_keeps_self_runtime_dependency = fun _ctx ->
  let app = make_package "app" in
  let workspace = make_workspace [ app ] in
  let graph =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~kind:(Build_unit_graph.Dev default_dev_artifacts)
        ())
  in
  assert_same_keys ~expected:[ library_key "app" ] ~actual:(Build_unit_graph.keys graph);
  Ok ()

let clone_keeps_dev_self_runtime_dependency = fun _ctx ->
  let app = make_package "app" in
  let workspace = make_workspace [ app ] in
  let first =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~kind:(Build_unit_graph.Dev default_dev_artifacts)
        ())
  in
  let second =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~kind:(Build_unit_graph.Dev default_dev_artifacts)
        ())
  in
  assert_same_keys ~expected:(Build_unit_graph.keys first) ~actual:(Build_unit_graph.keys second);
  Ok ()

let filter_for_unknown_package_returns_empty_graph = fun _ctx ->
  let packages = [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ] in
  let workspace = make_workspace packages in
  match Build_unit_graph.create workspace (request ~roots:[ package_name "does-not-exist" ] ()) with
  | Ok graph ->
      Test.assert_equal ~expected:0 ~actual:(Build_unit_graph.size graph);
      Ok ()
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Test.assert_equal
        ~expected:[ "root:does-not-exist" ]
        ~actual:(
          List.map
            missing
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Build_unit_graph.Root package -> "root:" ^ Package_name.to_string package
              | Dependency { package; dependency } ->
                  Package_name.to_string package ^ "->" ^ Package_name.to_string dependency)
        );
      Ok ()

let get_unplanned_dependencies_only_reports_unplanned_runtime_dependencies = fun _ctx ->
  let std = make_package "std" in
  let kernel = make_package ~dependencies:[ "std" ] "kernel" in
  let app = make_package ~dependencies:[ "kernel" ] "app" in
  let workspace = make_workspace [ std; kernel; app ] in
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_same_keys
    ~expected:[ library_key "kernel" ]
    ~actual:(dependency_keys_for_key graph (library_key "app"));
  assert_same_keys
    ~expected:[ library_key "std" ]
    ~actual:(dependency_keys_for_key graph (library_key "kernel"));
  Ok ()

let clone_preserves_edges_with_independent_node_status = fun _ctx ->
  let std = make_package "std" in
  let app = make_package ~dependencies:[ "std" ] "app" in
  let workspace = make_workspace [ std; app ] in
  let original = graph workspace (request ~roots:[ package_name "app" ] ()) in
  let recreated = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_same_keys
    ~expected:(dependency_keys_for_key original (library_key "app"))
    ~actual:(dependency_keys_for_key recreated (library_key "app"));
  Ok ()

let clone_reconstructs_planned_nested_graphs = fun _ctx ->
  let std = make_package "std" in
  let module_graph = Graph.SimpleGraph.make () in
  let root_module =
    Graph.SimpleGraph.add_node
      module_graph
      (Riot_planner.Module_node.make_ml
        (module_for_package std "src/std.ml")
        (Concrete (Path.v "src/std.ml")))
  in
  let child_module =
    Graph.SimpleGraph.add_node
      module_graph
      (Riot_planner.Module_node.make_ml
        (module_for_package std "src/child.ml")
        (Concrete (Path.v "src/child.ml")))
  in
  Riot_planner.Module_node.set_open_modules (G.value root_module) [ child_module ];
  Graph.SimpleGraph.add_edge root_module ~depends_on:child_module;
  let action_graph = Riot_planner.Action_graph.create () in
  let cloned_module_graph = Graph.SimpleGraph.make () in
  let cloned_root_node =
    Graph.SimpleGraph.add_node
      cloned_module_graph
      (Riot_planner.Module_node.make_ml
        (module_for_package std "src/std.ml")
        (Concrete (Path.v "src/std.ml")))
  in
  let cloned_child_node =
    Graph.SimpleGraph.add_node
      cloned_module_graph
      (Riot_planner.Module_node.make_ml
        (module_for_package std "src/child.ml")
        (Concrete (Path.v "src/child.ml")))
  in
  Riot_planner.Module_node.set_open_modules (G.value cloned_root_node) [ cloned_child_node ];
  Graph.SimpleGraph.add_edge cloned_root_node ~depends_on:cloned_child_node;
  let cloned_action_graph = Riot_planner.Action_graph.clone action_graph in
  let cloned_root = module_node_for_path cloned_module_graph "src/std.ml" in
  Riot_planner.Module_node.set_open_modules (G.value cloned_root) [];
  let _ = add_write_action cloned_action_graph std "generated.txt" "generated" in
  let original_root = module_node_for_path module_graph "src/std.ml" in
  Test.assert_equal ~expected:1 ~actual:(List.length (G.value original_root).open_modules);
  Test.assert_equal ~expected:0 ~actual:(List.length (G.value cloned_root).open_modules);
  Test.assert_equal
    ~expected:0
    ~actual:(List.length (Riot_planner.Action_graph.nodes action_graph));
  Test.assert_equal
    ~expected:1
    ~actual:(List.length (Riot_planner.Action_graph.nodes cloned_action_graph));
  Ok ()

let build_scope_wires_declared_build_dependencies = fun _ctx ->
  let packages = [ make_package "codegen"; make_package ~build_dependencies:[ "codegen" ] "app" ] in
  let workspace = make_workspace packages in
  let graph =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~synthetic_tools:[ Build_unit_graph.{ package = package_name "app"; name = "build" } ]
        ())
  in
  assert_same_keys
    ~expected:(library_keys [ "app"; "codegen" ])
    ~actual:(dependency_keys_for_key graph (synthetic_key "app"));
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
    case
      ~size:Large
      "build unit requests select expected artifacts"
      build_unit_requests_select_expected_artifacts;
    case "dev filter keeps self runtime dependency" dev_filter_keeps_self_runtime_dependency;
    case "clone keeps dev self runtime dependency" clone_keeps_dev_self_runtime_dependency;
    case
      "filter_for_unknown_package returns empty graph"
      filter_for_unknown_package_returns_empty_graph;
    case
      "get_unplanned_dependencies only reports unplanned runtime dependencies"
      get_unplanned_dependencies_only_reports_unplanned_runtime_dependencies;
    case
      "clone preserves edges with independent node status"
      clone_preserves_edges_with_independent_node_status;
    case
      "clone reconstructs planned nested graphs"
      clone_reconstructs_planned_nested_graphs;
    case
      "build scope wires declared build dependencies"
      build_scope_wires_declared_build_dependencies;
  ]

let name = "riot-planner:workspace-like-graph"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
