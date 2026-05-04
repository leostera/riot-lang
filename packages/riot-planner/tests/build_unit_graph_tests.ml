open Std
open Riot_model

module Test = Std.Test
module Build_unit = Riot_planner.Build_unit
module Build_unit_graph = Riot_planner.Build_unit_graph

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

let binary = fun ~name ~path -> Package.{ name; path = Path.v path }

let make_package = fun
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(binaries = [])
  ?(library = true)
  ?(workspace_member = true)
  name ->
  let library =
    if library then
      Some Package.{ path = Path.v "src/lib.ml" }
    else
      None
  in
  let relative_path =
    if workspace_member then
      Path.v ("packages/" ^ name)
    else
      Path.v ("../registry/" ^ name)
  in
  Package.make
    ~name:(package_name name)
    ~path:(Path.v ("packages/" ^ name))
    ~relative_path
    ~dependencies:(List.map dependencies ~fn:dependency)
    ~dev_dependencies:(List.map dev_dependencies ~fn:dependency)
    ~build_dependencies:(List.map build_dependencies ~fn:dependency)
    ~binaries
    ?library
    ()

let make_workspace = fun packages ->
  Workspace.make_realized
    ~root:(Path.v "/tmp/build_unit_graph_tests")
    ~packages
    ()

let linux_target =
  Target.from_string "x86_64-unknown-linux-gnu"
  |> Result.expect ~msg:"expected linux target"

let macos_target =
  Target.from_string "aarch64-apple-darwin"
  |> Result.expect ~msg:"expected macos target"

let default_dev_artifacts = Package.{ tests = true; examples = true; benches = true }

let request = fun
  ?roots
  ?(targets = [macos_target])
  ?(profile = Profile.debug)
  ?(kind = Build_unit_graph.Runtime)
  ?(synthetic_tools = [])
  () ->
  Build_unit_graph.{
    roots;
    targets;
    profile;
    kind;
    synthetic_tools;
  }

let library_key = fun ?(target = macos_target) package ->
  Build_unit.{
    package = package_name package;
    artifact = Library;
    target;
    profile = Profile.debug;
  }

let runtime_binary_key = fun ?(target = macos_target) package name ->
  Build_unit.{
    package = package_name package;
    artifact = RuntimeBinary { name };
    target;
    profile = Profile.debug;
  }

let test_binary_key = fun ?(target = macos_target) package name ->
  Build_unit.{
    package = package_name package;
    artifact = TestBinary { name };
    target;
    profile = Profile.debug;
  }

let example_binary_key = fun ?(target = macos_target) package name ->
  Build_unit.{
    package = package_name package;
    artifact = ExampleBinary { name };
    target;
    profile = Profile.debug;
  }

let bench_binary_key = fun ?(target = macos_target) package name ->
  Build_unit.{
    package = package_name package;
    artifact = BenchBinary { name };
    target;
    profile = Profile.debug;
  }

let synthetic_key = fun ?(target = Target.host ()) package name ->
  Build_unit.{
    package = package_name package;
    artifact = SyntheticTool { name };
    target;
    profile = Profile.debug;
  }

let sort_keys = fun keys -> List.sort keys ~compare:Build_unit.compare_key

let assert_keys_equal = fun ~expected ~actual ->
  Test.assert_equal
    ~expected:(
      sort_keys expected
      |> List.map ~fn:Build_unit.key_to_string
    )
    ~actual:(
      sort_keys actual
      |> List.map ~fn:Build_unit.key_to_string
    )

let graph = fun workspace request ->
  Build_unit_graph.create workspace request
  |> Result.expect ~msg:"expected build unit graph"

let dependencies = fun graph key -> Build_unit_graph.dependencies graph key

let missing_package_to_string = fun __tmp1 ->
  match __tmp1 with
  | Build_unit_graph.Root package -> "root:" ^ Package_name.to_string package
  | Dependency { package; dependency } ->
      "dependency:" ^ Package_name.to_string package ^ "->" ^ Package_name.to_string dependency

let runtime_roots_build_runtime_artifacts_and_dependency_libraries = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package
          ~dependencies:[ "std" ]
          ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
          "app";
      ]
  in
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_keys_equal
    ~expected:[ library_key "std"; library_key "app"; runtime_binary_key "app" "app"; ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal ~expected:[ library_key "std" ] ~actual:(dependencies graph (library_key "app"));
  assert_keys_equal
    ~expected:[ library_key "app"; library_key "std" ]
    ~actual:(dependencies graph (runtime_binary_key "app" "app"));
  Ok ()

let dev_roots_build_selected_dev_artifacts_but_not_dependency_tests = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package ~binaries:[ binary ~name:"std-tests" ~path:"tests/std_tests.ml" ] "std";
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
  let graph =
    graph
      workspace
      (request ~roots:[ package_name "app" ] ~kind:(Build_unit_graph.Dev default_dev_artifacts) ())
  in
  assert_keys_equal
    ~expected:[
      library_key "std";
      library_key "propane";
      library_key "app";
      test_binary_key "app" "app-tests";
      example_binary_key "app" "demo";
      bench_binary_key "app" "perf";
    ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal
    ~expected:[ library_key "app"; library_key "std"; library_key "propane" ]
    ~actual:(dependencies graph (test_binary_key "app" "app-tests"));
  Ok ()

let dev_artifact_flags_filter_requested_binary_kinds = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package
          ~binaries:[
            binary ~name:"app-tests" ~path:"tests/app_tests.ml";
            binary ~name:"demo" ~path:"examples/demo.ml";
            binary ~name:"perf" ~path:"bench/perf.ml";
          ]
          "app";
      ]
  in
  let graph =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~kind:(Build_unit_graph.Dev Package.{ tests = true; examples = false; benches = false })
        ())
  in
  assert_keys_equal
    ~expected:[ library_key "app"; test_binary_key "app" "app-tests" ]
    ~actual:(Build_unit_graph.keys graph);
  Ok ()

let runtime_roots_ignore_dev_only_binaries = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package
          ~binaries:[
            binary ~name:"app" ~path:"src/app.ml";
            binary ~name:"app-tests" ~path:"tests/app_tests.ml";
            binary ~name:"demo" ~path:"examples/demo.ml";
            binary ~name:"perf" ~path:"bench/perf.ml";
          ]
          "app";
      ]
  in
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_keys_equal
    ~expected:[ library_key "app"; runtime_binary_key "app" "app" ]
    ~actual:(Build_unit_graph.keys graph);
  Ok ()

let multi_target_requests_create_disconnected_target_islands = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package
          ~dependencies:[ "std" ]
          ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
          "app";
      ]
  in
  let graph =
    graph
      workspace
      (request ~roots:[ package_name "app" ] ~targets:[ macos_target; linux_target ] ())
  in
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
  assert_keys_equal
    ~expected:[ library_key ~target:macos_target "std" ]
    ~actual:(dependencies graph (library_key ~target:macos_target "app"));
  assert_keys_equal
    ~expected:[ library_key ~target:linux_target "std" ]
    ~actual:(dependencies graph (library_key ~target:linux_target "app"));
  Ok ()

let package_without_library_does_not_create_self_library_dependency = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "propane";
        make_package
          ~library:false
          ~dev_dependencies:[ "propane" ]
          ~binaries:[ binary ~name:"app-tests" ~path:"tests/app_tests.ml" ]
          "app";
      ]
  in
  let graph =
    graph
      workspace
      (request ~roots:[ package_name "app" ] ~kind:(Build_unit_graph.Dev default_dev_artifacts) ())
  in
  assert_keys_equal
    ~expected:[ library_key "propane"; test_binary_key "app" "app-tests" ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal
    ~expected:[ library_key "propane" ]
    ~actual:(dependencies graph (test_binary_key "app" "app-tests"));
  Ok ()

let runtime_binary_without_library_depends_on_runtime_dependencies = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package
          ~library:false
          ~dependencies:[ "std" ]
          ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
          "app";
      ]
  in
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_keys_equal
    ~expected:[ library_key "std"; runtime_binary_key "app" "app" ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal
    ~expected:[ library_key "std" ]
    ~actual:(dependencies graph (runtime_binary_key "app" "app"));
  Ok ()

let build_dependencies_are_host_library_requirements = fun _ctx ->
  let host_target = Target.host () in
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "codegen";
        make_package ~build_dependencies:[ "codegen" ] "app";
      ]
  in
  let graph =
    graph workspace (request ~roots:[ package_name "app" ] ~targets:[ linux_target ] ())
  in
  assert_keys_equal
    ~expected:[
      library_key ~target:linux_target "app";
      library_key ~target:host_target "codegen";
      library_key ~target:host_target "std";
    ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal
    ~expected:[ library_key ~target:host_target "codegen" ]
    ~actual:(dependencies graph (library_key ~target:linux_target "app"));
  assert_keys_equal
    ~expected:[ library_key ~target:host_target "std" ]
    ~actual:(dependencies graph (library_key ~target:host_target "codegen"));
  Ok ()

let build_dependencies_connect_target_islands_to_shared_host_requirements = fun _ctx ->
  let host_target = Target.host () in
  let workspace =
    make_workspace [ make_package "codegen"; make_package ~build_dependencies:[ "codegen" ] "app"; ]
  in
  let graph =
    graph
      workspace
      (request ~roots:[ package_name "app" ] ~targets:[ macos_target; linux_target ] ())
  in
  assert_keys_equal
    ~expected:[
      library_key ~target:macos_target "app";
      library_key ~target:linux_target "app";
      library_key ~target:host_target "codegen";
    ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal
    ~expected:[ library_key ~target:host_target "codegen" ]
    ~actual:(dependencies graph (library_key ~target:macos_target "app"));
  assert_keys_equal
    ~expected:[ library_key ~target:host_target "codegen" ]
    ~actual:(dependencies graph (library_key ~target:linux_target "app"));
  Test.assert_false
    (Option.is_some (Build_unit_graph.find graph (library_key ~target:linux_target "codegen")));
  Ok ()

let synthetic_tools_are_host_only_build_units = fun _ctx ->
  let host_target = Target.host () in
  let workspace =
    make_workspace [ make_package "std"; make_package ~dependencies:[ "std" ] "fixme"; ]
  in
  let graph =
    graph
      workspace
      (request
        ~roots:[ package_name "fixme" ]
        ~targets:[ macos_target; linux_target ]
        ~synthetic_tools:[
          Build_unit_graph.{ package = package_name "fixme"; name = "fixme-runner" };
        ]
        ())
  in
  assert_keys_equal
    ~expected:[
      library_key ~target:macos_target "fixme";
      library_key ~target:linux_target "fixme";
      library_key ~target:macos_target "std";
      library_key ~target:linux_target "std";
      synthetic_key "fixme" "fixme-runner";
    ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal
    ~expected:[ library_key ~target:host_target "fixme"; library_key ~target:host_target "std" ]
    ~actual:(dependencies graph (synthetic_key "fixme" "fixme-runner"));
  Ok ()

let missing_dependency_reports_package_and_dependency = fun _ctx ->
  let workspace = make_workspace [ make_package ~dependencies:[ "missing" ] "app"; ] in
  match Build_unit_graph.create workspace (request ~roots:[ package_name "app" ] ()) with
  | Ok _ -> Error "expected missing dependency"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      let labels =
        missing
        |> List.map ~fn:missing_package_to_string
        |> List.sort ~compare:String.compare
      in
      Test.assert_equal ~expected:[ "dependency:app->missing" ] ~actual:labels;
      Ok ()

let missing_root_reports_root_package = fun _ctx ->
  let workspace = make_workspace [ make_package "std" ] in
  match Build_unit_graph.create workspace (request ~roots:[ package_name "app" ] ()) with
  | Ok _ -> Error "expected missing root"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Test.assert_equal
        ~expected:[ "root:app" ]
        ~actual:(
          missing
          |> List.map ~fn:missing_package_to_string
          |> List.sort ~compare:String.compare
        );
      Ok ()

let missing_synthetic_tool_package_reports_root_package = fun _ctx ->
  let workspace = make_workspace [ make_package "app" ] in
  match Build_unit_graph.create
    workspace
    (request
      ~roots:[ package_name "app" ]
      ~synthetic_tools:[
        Build_unit_graph.{ package = package_name "fixme"; name = "fixme-runner" };
      ]
      ()) with
  | Ok _ -> Error "expected missing synthetic package"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Test.assert_equal
        ~expected:[ "root:fixme" ]
        ~actual:(
          missing
          |> List.map ~fn:missing_package_to_string
          |> List.sort ~compare:String.compare
        );
      Ok ()

let builtin_dependencies_do_not_create_missing_packages_or_graph_nodes = fun _ctx ->
  let workspace = make_workspace [ make_package ~dependencies:[ "unix" ] "app"; ] in
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_keys_equal ~expected:[ library_key "app" ] ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal ~expected:[] ~actual:(dependencies graph (library_key "app"));
  Ok ()

let duplicate_dependencies_produce_single_dependency_edge = fun _ctx ->
  let workspace =
    make_workspace [ make_package "std"; make_package ~dependencies:[ "std"; "std" ] "app"; ]
  in
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  assert_keys_equal ~expected:[ library_key "std" ] ~actual:(dependencies graph (library_key "app"));
  Ok ()

let duplicate_missing_dependencies_are_reported_once = fun _ctx ->
  let workspace = make_workspace [ make_package ~dependencies:[ "missing"; "missing" ] "app"; ] in
  match Build_unit_graph.create workspace (request ~roots:[ package_name "app" ] ()) with
  | Ok _ -> Error "expected missing dependency"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Test.assert_equal
        ~expected:[ "dependency:app->missing" ]
        ~actual:(
          missing
          |> List.map ~fn:missing_package_to_string
          |> List.sort ~compare:String.compare
        );
      Ok ()

let roots_none_selects_workspace_members_and_uses_external_dependencies_as_libraries = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package
          ~workspace_member:false
          ~binaries:[ binary ~name:"std-tests" ~path:"tests/std_tests.ml" ]
          "std";
        make_package
          ~dependencies:[ "std" ]
          ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
          "app";
      ]
  in
  let graph = graph workspace (request ()) in
  assert_keys_equal
    ~expected:[ library_key "std"; library_key "app"; runtime_binary_key "app" "app"; ]
    ~actual:(Build_unit_graph.keys graph);
  Ok ()

let multiple_roots_share_dependency_nodes = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "app";
        make_package ~dependencies:[ "std" ] "tool";
      ]
  in
  let graph = graph workspace (request ~roots:[ package_name "app"; package_name "tool" ] ()) in
  assert_keys_equal
    ~expected:[ library_key "std"; library_key "app"; library_key "tool" ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal ~expected:[ library_key "std" ] ~actual:(dependencies graph (library_key "app"));
  assert_keys_equal
    ~expected:[ library_key "std" ]
    ~actual:(dependencies graph (library_key "tool"));
  Ok ()

let profile_participates_in_build_unit_identity = fun _ctx ->
  let workspace = make_workspace [ make_package "app" ] in
  let graph = graph workspace (request ~profile:Profile.release ~roots:[ package_name "app" ] ()) in
  let release_key =
    Build_unit.{
      package = package_name "app";
      artifact = Library;
      target = macos_target;
      profile = Profile.release;
    }
  in
  assert_keys_equal ~expected:[ release_key ] ~actual:(Build_unit_graph.keys graph);
  Test.assert_equal
    ~expected:"app:library:aarch64-apple-darwin:release"
    ~actual:(Build_unit.key_to_string release_key);
  Ok ()

let dev_dependency_runtime_closure_uses_the_requested_target = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "propane";
        make_package
          ~dev_dependencies:[ "propane" ]
          ~binaries:[ binary ~name:"app-tests" ~path:"tests/app_tests.ml" ]
          "app";
      ]
  in
  let graph =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~targets:[ linux_target ]
        ~kind:(Build_unit_graph.Dev default_dev_artifacts)
        ())
  in
  assert_keys_equal
    ~expected:[
      library_key ~target:linux_target "std";
      library_key ~target:linux_target "propane";
      library_key ~target:linux_target "app";
      test_binary_key ~target:linux_target "app" "app-tests";
    ]
    ~actual:(Build_unit_graph.keys graph);
  assert_keys_equal
    ~expected:[ library_key ~target:linux_target "std" ]
    ~actual:(dependencies graph (library_key ~target:linux_target "propane"));
  assert_keys_equal
    ~expected:[
      library_key ~target:linux_target "app";
      library_key ~target:linux_target "propane";
    ]
    ~actual:(dependencies graph (test_binary_key ~target:linux_target "app" "app-tests"));
  Ok ()

let topological_sort_places_libraries_before_consumers = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package
          ~dependencies:[ "std" ]
          ~binaries:[ binary ~name:"app" ~path:"src/app.ml" ]
          "app";
      ]
  in
  let graph = graph workspace (request ~roots:[ package_name "app" ] ()) in
  let sorted =
    Build_unit_graph.topological_sort graph
    |> Result.expect ~msg:"expected acyclic build unit graph"
    |> List.map ~fn:(fun unit -> unit.Build_unit.key)
  in
  let position key =
    List.enumerate sorted
    |> List.find ~fn:(fun (_, current) -> Build_unit.equal_key current key)
    |> Option.map ~fn:(fun (index, _) -> index)
    |> Option.expect ~msg:("missing key: " ^ Build_unit.key_to_string key)
  in
  Test.assert_true (position (library_key "std") < position (library_key "app"));
  Test.assert_true (position (library_key "app") < position (runtime_binary_key "app" "app"));
  Ok ()

let topological_sort_reports_dependency_cycles = fun _ctx ->
  let workspace =
    make_workspace
      [ make_package ~dependencies:[ "b" ] "a"; make_package ~dependencies:[ "a" ] "b"; ]
  in
  let graph = graph workspace (request ~roots:[ package_name "a" ] ()) in
  match Build_unit_graph.topological_sort graph with
  | Ok _ -> Error "expected dependency cycle"
  | Error cycle ->
      Test.assert_true (not (List.is_empty cycle));
      Ok ()

let dependency_edges_only_reference_graph_nodes = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "codegen";
        make_package "propane";
        make_package
          ~dependencies:[ "std" ]
          ~dev_dependencies:[ "propane" ]
          ~build_dependencies:[ "codegen" ]
          ~binaries:[
            binary ~name:"app" ~path:"src/app.ml";
            binary ~name:"app-tests" ~path:"tests/app_tests.ml";
          ]
          "app";
      ]
  in
  let graph =
    graph
      workspace
      (request
        ~roots:[ package_name "app" ]
        ~targets:[ macos_target; linux_target ]
        ~kind:(Build_unit_graph.Dev default_dev_artifacts)
        ~synthetic_tools:[
          Build_unit_graph.{ package = package_name "app"; name = "fixme-runner" };
        ]
        ())
  in
  List.for_each
    (Build_unit_graph.keys graph)
    ~fn:(fun key ->
      List.for_each
        (Build_unit_graph.dependencies graph key)
        ~fn:(fun dependency ->
          Test.assert_true
            (Option.is_some (Build_unit_graph.find graph dependency))));
  Ok ()

let tests =
  Test.[
    case
      "build unit runtime roots build runtime artifacts and dependency libraries"
      runtime_roots_build_runtime_artifacts_and_dependency_libraries;
    case
      "build unit dev roots build selected dev artifacts but not dependency tests"
      dev_roots_build_selected_dev_artifacts_but_not_dependency_tests;
    case
      "build unit dev artifact flags filter requested binary kinds"
      dev_artifact_flags_filter_requested_binary_kinds;
    case "build unit runtime roots ignore dev-only binaries" runtime_roots_ignore_dev_only_binaries;
    case
      "build unit multi target requests create disconnected target islands"
      multi_target_requests_create_disconnected_target_islands;
    case
      "build unit package without library does not create self library dependency"
      package_without_library_does_not_create_self_library_dependency;
    case
      "build unit runtime binary without library depends on runtime dependencies"
      runtime_binary_without_library_depends_on_runtime_dependencies;
    case
      "build unit build dependencies are host library requirements"
      build_dependencies_are_host_library_requirements;
    case
      "build unit build dependencies connect target islands to shared host requirements"
      build_dependencies_connect_target_islands_to_shared_host_requirements;
    case
      "build unit synthetic tools are host only build units"
      synthetic_tools_are_host_only_build_units;
    case
      "build unit missing dependency reports package and dependency"
      missing_dependency_reports_package_and_dependency;
    case "build unit missing root reports root package" missing_root_reports_root_package;
    case
      "build unit missing synthetic tool package reports root package"
      missing_synthetic_tool_package_reports_root_package;
    case
      "build unit builtin dependencies do not create missing packages or graph nodes"
      builtin_dependencies_do_not_create_missing_packages_or_graph_nodes;
    case
      "build unit duplicate dependencies produce single dependency edge"
      duplicate_dependencies_produce_single_dependency_edge;
    case
      "build unit duplicate missing dependencies are reported once"
      duplicate_missing_dependencies_are_reported_once;
    case
      "build unit roots none selects workspace members and uses external dependencies as libraries"
      roots_none_selects_workspace_members_and_uses_external_dependencies_as_libraries;
    case "build unit multiple roots share dependency nodes" multiple_roots_share_dependency_nodes;
    case
      "build unit profile participates in build unit identity"
      profile_participates_in_build_unit_identity;
    case
      "build unit dev dependency runtime closure uses the requested target"
      dev_dependency_runtime_closure_uses_the_requested_target;
    case
      "build unit topological sort places libraries before consumers"
      topological_sort_places_libraries_before_consumers;
    case
      "build unit topological sort reports dependency cycles"
      topological_sort_reports_dependency_cycles;
    case
      "build unit dependency edges only reference graph nodes"
      dependency_edges_only_reference_graph_nodes;
  ]

let name = "riot-planner:build-unit-graph"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
