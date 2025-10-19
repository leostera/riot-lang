open Std
open Tusk_planner
open Tusk_model

module G = Std.Graph.SimpleGraph

let make_test_config root_path source_dir =
  Graph_builder.{
    root = root_path;
    source_dir;
    namespace = "Test";
    package = Package.{
      name = "test";
      path = root_path;
      relative_path = Path.v ".";
      dependencies = [];
      binaries = [];
    };
    toolchain = Toolchains.default_toolchain;
    workspace = Workspace.{
      root = Path.v ".";
      target_dir_root = Path.v "_build";
      packages = [];
    };
  }

let test_graph_has_root_node () =
  let config = make_test_config (Path.v "tests/fixtures/simple") (Path.v "src") in
  let _graph = Graph_builder.create config in
  Ok ()

let test_graph_namespace_is_set () =
  let config = make_test_config (Path.v "tests/fixtures/simple") (Path.v "src") in
  let graph = Graph_builder.create config in
  if graph.config.namespace = "Test" then Ok ()
  else Error (format "Expected namespace 'Test' but got '%s'" graph.config.namespace)

let test_planner_generates_actions () =
  let root = Path.v "tests/fixtures/simple" in
  let source_dir = Path.v "src" in
  let sandbox_dir = Path.v "_build/test-sandbox" in
  
  let input = Tusk_planner.{
    package = Package.{
      name = "test";
      path = root;
      relative_path = Path.v ".";
      dependencies = [];
      binaries = [];
    };
    toolchain = Toolchains.default_toolchain;
    workspace = Workspace.{
      root = Path.v ".";
      target_dir_root = Path.v "_build";
      packages = [];
    };
    source_dir;
    sandbox_dir;
    dependencies = [];
  } in
  
  match Tusk_planner.plan_node input with
  | Planned { action_graph; outputs; _ } ->
      let actions = Action_graph.to_action_list action_graph in
      if List.length actions > 0 && List.length outputs > 0 then Ok ()
      else Error (format "Expected actions and outputs, got %d actions and %d outputs" 
        (List.length actions) (List.length outputs))
  | Cycle { cycle } ->
      Error (format "Unexpected cycle: %s" (String.concat " -> " cycle))
  | Error msg ->
      Error (format "Planning failed: %s" msg)

let tests = Test.[
  case "graph has root node" test_graph_has_root_node;
  case "graph namespace is set" test_graph_namespace_is_set;
  case "planner generates actions" test_planner_generates_actions;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"Graph Builder Tests" ~tests ~args ())
    ~args:Env.args
  |> exit
