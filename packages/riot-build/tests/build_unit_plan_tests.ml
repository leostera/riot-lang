open Std

module Test = Std.Test
module Build_context = Riot_build.Internal.Build_context
module Build_unit = Riot_planner.Build_unit
module Build_unit_graph = Riot_planner.Build_unit_graph
module Build_unit_plan = Riot_build.Internal.Build_unit_plan
module Resolved_build = Riot_build.Internal.Resolved_build

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let macos_target = target "aarch64-apple-darwin"

let linux_target = target "x86_64-unknown-linux-gnu"

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

let binary = fun ~name ~path -> Riot_model.Package.{ name; path = Path.v path }

let make_package = fun
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(binaries = [])
  ?(library = true)
  name ->
  let library =
    if library then
      Some Riot_model.Package.{ path = Path.v "src/lib.ml" }
    else
      None
  in
  Riot_model.Package.make
    ~name:(package_name name)
    ~path:(Path.v ("packages/" ^ name))
    ~relative_path:(Path.v ("packages/" ^ name))
    ~dependencies:(List.map dependencies ~fn:package_dependency)
    ~dev_dependencies:(List.map dev_dependencies ~fn:package_dependency)
    ~build_dependencies:(List.map build_dependencies ~fn:package_dependency)
    ~binaries
    ?library
    ()

let make_workspace = fun ~root packages -> Riot_model.Workspace.make_realized ~root ~packages ()

let request = fun
  ?(scope = Riot_build.Request.Runtime)
  ?(dev_artifacts = Riot_model.Package.{tests = true; examples = true; benches = true;})
  ~workspace
  ~package_names
  ~targets
  () ->
  Riot_build.Request.make
    ~workspace
    ~packages:package_names
    ~targets:(Riot_model.Target.Exact (Riot_model.Target.Set.from_list targets))
    ~scope
    ~dev_artifacts
    ~profile:Riot_model.Profile.debug
    ()

let context_and_resolved = fun request ->
  let context =
    Build_context.make request
    |> Result.expect ~msg:"expected build context"
  in
  let resolved =
    Resolved_build.resolve context request
    |> Result.expect ~msg:"expected resolved build"
  in
  (context, resolved)

let library_key = fun ?(target = macos_target) package ->
  ({
    package = package_name package;
    artifact = Build_unit.Library;
    target;
    profile = Riot_model.Profile.debug;
  }:Build_unit.key)

let runtime_binary_key = fun ?(target = macos_target) package name ->
  ({
    package = package_name package;
    artifact = Build_unit.RuntimeBinary { name };
    target;
    profile = Riot_model.Profile.debug;
  }:Build_unit.key)

let test_binary_key = fun ?(target = macos_target) package name ->
  ({
    package = package_name package;
    artifact = Build_unit.TestBinary { name };
    target;
    profile = Riot_model.Profile.debug;
  }:Build_unit.key)

let example_binary_key = fun ?(target = macos_target) package name ->
  ({
    package = package_name package;
    artifact = Build_unit.ExampleBinary { name };
    target;
    profile = Riot_model.Profile.debug;
  }:Build_unit.key)

let bench_binary_key = fun ?(target = macos_target) package name ->
  ({
    package = package_name package;
    artifact = Build_unit.BenchBinary { name };
    target;
    profile = Riot_model.Profile.debug;
  }:Build_unit.key)

let synthetic_key = fun ?(target = Riot_model.Target.host ()) package name ->
  ({
    package = package_name package;
    artifact = Build_unit.SyntheticTool { name };
    target;
    profile = Riot_model.Profile.debug;
  }:Build_unit.key)

let assert_keys_equal = fun ~expected ~actual ->
  let normalize keys =
    keys
    |> List.sort ~compare:Build_unit.compare_key
    |> List.map ~fn:Build_unit.key_to_string
  in
  Test.assert_equal ~expected:(normalize expected) ~actual:(normalize actual)

let build_graph = fun ?synthetic_tools context resolved ->
  Build_unit_plan.create_graph ?synthetic_tools context resolved
  |> Result.expect ~msg:"expected build unit graph"

let build_plan = fun ?synthetic_tools context resolved ->
  Build_unit_plan.create ?synthetic_tools context resolved
  |> Result.expect ~msg:"expected build unit plan"

let runtime_request_uses_resolved_roots_targets_and_profile = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_unit_plan_runtime"
    (fun root ->
      let workspace =
        make_workspace
          ~root
          [
            make_package "std";
            make_package
              ~dependencies:[ "std" ]
              ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
              "app";
          ]
      in
      let (context, resolved) =
        request
          ~workspace
          ~package_names:[ package_name "app" ]
          ~targets:[ macos_target; linux_target ]
          ()
        |> context_and_resolved
      in
      let graph = build_graph context resolved in
      assert_keys_equal
        ~expected:[
          library_key ~target:macos_target "std";
          library_key ~target:macos_target "app";
          runtime_binary_key ~target:macos_target "app" "app";
          library_key ~target:linux_target "std";
          library_key ~target:linux_target "app";
          runtime_binary_key ~target:linux_target "app" "app";
        ]
        ~actual:(Build_unit_graph.keys graph);
      Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let dev_request_preserves_selected_artifact_flags = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_unit_plan_dev"
    (fun root ->
      let workspace =
        make_workspace
          ~root
          [
            make_package "std";
            make_package "propane";
            make_package
              ~dependencies:[ "std" ]
              ~dev_dependencies:[ "propane" ]
              ~binaries:[
                binary ~name:"app-tests" ~path:"tests/app_tests.ml";
                binary ~name:"demo" ~path:"examples/demo.ml";
                binary ~name:"perf" ~path:"bench/perf.ml";
              ]
              "app";
          ]
      in
      let (context, resolved) =
        request
          ~workspace
          ~scope:Riot_build.Request.Dev
          ~dev_artifacts:Riot_model.Package.{ tests = true; examples = false; benches = false }
          ~package_names:[ package_name "app" ]
          ~targets:[ macos_target ]
          ()
        |> context_and_resolved
      in
      let graph = build_graph context resolved in
      assert_keys_equal
        ~expected:[
          library_key "std";
          library_key "propane";
          library_key "app";
          test_binary_key "app" "app-tests";
        ]
        ~actual:(Build_unit_graph.keys graph);
      Test.assert_true
        (not
          (List.any
            (Build_unit_graph.keys graph)
            ~fn:(fun key -> Build_unit.equal_key key (example_binary_key "app" "demo"))));
      Test.assert_true
        (not
          (List.any
            (Build_unit_graph.keys graph)
            ~fn:(fun key -> Build_unit.equal_key key (bench_binary_key "app" "perf"))));
      Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let synthetic_tools_are_host_units_in_the_same_graph = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_unit_plan_synthetic"
    (fun root ->
      let workspace =
        make_workspace
          ~root
          [
            make_package "std";
            make_package
              ~dependencies:[ "std" ]
              ~build_dependencies:[ "std" ]
              ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
              "app";
          ]
      in
      let (context, resolved) =
        request ~workspace ~package_names:[ package_name "app" ] ~targets:[ linux_target ] ()
        |> context_and_resolved
      in
      let synthetic_tools = [
        Build_unit_graph.{ package = package_name "app"; name = "fixme-runner" };
      ]
      in
      let graph = build_graph ~synthetic_tools context resolved in
      assert_keys_equal
        ~expected:[
          library_key ~target:linux_target "std";
          library_key ~target:linux_target "app";
          runtime_binary_key ~target:linux_target "app" "app";
          library_key ~target:(Riot_model.Target.host ()) "std";
          library_key ~target:(Riot_model.Target.host ()) "app";
          synthetic_key "app" "fixme-runner";
        ]
        ~actual:(Build_unit_graph.keys graph);
      assert_keys_equal
        ~expected:[
          library_key ~target:(Riot_model.Target.host ()) "app";
          library_key ~target:(Riot_model.Target.host ()) "std";
        ]
        ~actual:(Build_unit_graph.dependencies graph (synthetic_key "app" "fixme-runner"));
      Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let build_unit_plan_stores_topologically_sorted_units = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_unit_plan_sorted"
    (fun root ->
      let workspace =
        make_workspace
          ~root
          [
            make_package "std";
            make_package
              ~dependencies:[ "std" ]
              ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
              "app";
          ]
      in
      let (context, resolved) =
        request ~workspace ~package_names:[ package_name "app" ] ~targets:[ macos_target ] ()
        |> context_and_resolved
      in
      let plan = build_plan context resolved in
      let keys =
        Build_unit_plan.units plan
        |> List.map ~fn:Build_unit.key
      in
      let position key =
        List.enumerate keys
        |> List.find ~fn:(fun (_, current) -> Build_unit.equal_key current key)
        |> Option.map ~fn:(fun (index, _) -> index)
        |> Option.expect ~msg:("missing key: " ^ Build_unit.key_to_string key)
      in
      Test.assert_true (position (library_key "std") < position (library_key "app"));
      Test.assert_true (position (library_key "app") < position (runtime_binary_key "app" "app"));
      Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let build_unit_graph_clone_owns_its_nodes = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_unit_plan_clone"
    (fun root ->
      let workspace =
        make_workspace
          ~root
          [
            make_package "std";
            make_package
              ~dependencies:[ "std" ]
              ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
              "app";
          ]
      in
      let (context, resolved) =
        request
          ~workspace
          ~package_names:[ package_name "app" ]
          ~targets:[ macos_target; linux_target ]
          ()
        |> context_and_resolved
      in
      let plan = build_plan context resolved in
      let original_graph = Build_unit_plan.graph plan in
      let cloned_graph = Build_unit_graph.clone original_graph in
      if Ptr.equal original_graph cloned_graph then
        Error "expected cloned build unit graph to own a distinct graph"
      else (
        assert_keys_equal
          ~expected:(Build_unit_graph.keys original_graph)
          ~actual:(Build_unit_graph.keys cloned_graph);
        Test.assert_equal
          ~expected:(
            Build_unit_graph.dependencies
              original_graph
              (runtime_binary_key ~target:linux_target "app" "app")
            |> List.map ~fn:Build_unit.key_to_string
          )
          ~actual:(
            Build_unit_graph.dependencies
              cloned_graph
              (runtime_binary_key ~target:linux_target "app" "app")
            |> List.map ~fn:Build_unit.key_to_string
          );
        Ok ()
      )) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests =
  Test.[
    case
      "build unit plan translates runtime roots targets and profile"
      runtime_request_uses_resolved_roots_targets_and_profile;
    case
      "build unit plan preserves selected dev artifact flags"
      dev_request_preserves_selected_artifact_flags;
    case
      "build unit plan includes synthetic tools in the same graph"
      synthetic_tools_are_host_units_in_the_same_graph;
    case
      "build unit plan stores topologically sorted units"
      build_unit_plan_stores_topologically_sorted_units;
    case
      "build unit graph clone owns its nodes"
      build_unit_graph_clone_owns_its_nodes;
  ]

let name = "Riot Build Unit Plan Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
