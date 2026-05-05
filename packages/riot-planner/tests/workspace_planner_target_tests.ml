open Std
open Riot_model

module Test = Std.Test
module Build_unit = Riot_planner.Build_unit
module Build_unit_graph = Riot_planner.Build_unit_graph
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
  ?(workspace_member = true)
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(binaries = [])
  name ->
  let relative_path =
    if workspace_member then
      Path.v ("packages/" ^ name)
    else
      Path.v ("../deps/" ^ name)
  in
  Package.make
    ~name:(package_name name)
    ~path:(
      if workspace_member then
        Path.v ("packages/" ^ name)
      else
        Path.v ("/tmp/deps/" ^ name)
    )
    ~relative_path
    ~dependencies:(List.map dependencies ~fn:dependency)
    ~dev_dependencies:(List.map dev_dependencies ~fn:dependency)
    ~build_dependencies:(List.map build_dependencies ~fn:dependency)
    ~binaries
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let test_binary = fun name -> Package.{ name; path = Path.v ("tests/" ^ name ^ ".ml") }

let make_workspace = fun packages ->
  Workspace.make_realized
    ~root:(Path.v "/tmp/workspace_planner_target_tests")
    ~packages
    ()

let host_target = Target.host ()

let default_dev_artifacts = Package.{ tests = true; examples = true; benches = true }

let request = fun ?roots ?(kind = Build_unit_graph.Runtime) () ->
  Build_unit_graph.{
    roots;
    targets = [ host_target ];
    profile = Profile.debug;
    kind;
    synthetic_tools = [];
  }

let plan_build_units = fun workspace request ->
  let graph = Build_unit_graph.create workspace request in
  match graph with
  | Error _ as err -> err
  | Ok graph -> (
      match Build_unit_graph.topological_sort graph with
      | Ok units -> Ok (graph, units)
      | Error cycle ->
          let cycle_message =
            cycle
            |> List.map ~fn:Build_unit.key_to_string
            |> String.concat " -> "
          in
          Error (Build_unit_graph.MissingPackages {
            missing = [ Root (package_name ("cycle:" ^ cycle_message)) ];
          })
    )

let package_names = fun units ->
  units
  |> List.map ~fn:(fun (unit: Build_unit.t) -> (Build_unit.key unit).package)
  |> List.unique ~compare:Package_name.compare
  |> List.sort ~compare:Package_name.compare

let compact_key = fun (key: Build_unit.key) ->
  let package = Package_name.to_string key.package in
  match key.artifact with
  | Library -> package ^ ":runtime"
  | RuntimeBinary { name } -> package ^ ":bin:" ^ name
  | TestBinary { name } -> package ^ ":test:" ^ name
  | ExampleBinary { name } -> package ^ ":example:" ^ name
  | BenchBinary { name } -> package ^ ":bench:" ^ name
  | SyntheticTool { name } -> package ^ ":build:" ^ name

let package_keys = fun units ->
  units
  |> List.map ~fn:(fun (unit: Build_unit.t) -> compact_key (Build_unit.key unit))

let missing_to_string = fun __tmp1 ->
  match __tmp1 with
  | Build_unit_graph.Root package -> "root:" ^ Package_name.to_string package
  | Dependency { package; dependency } ->
      Package_name.to_string package ^ "->" ^ Package_name.to_string dependency

let plan_all_runtime_returns_workspace_like_order = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "kernel";
        make_package ~dependencies:[ "std"; "kernel" ] "actors";
        make_package ~dependencies:[ "std"; "actors" ] "riot-model";
        make_package ~dependencies:[ "std"; "riot-model" ] "riot-planner";
        make_package ~dependencies:[ "std"; "riot-model"; "riot-planner" ] "riot-store";
        make_package
          ~dependencies:[ "std"; "riot-model"; "riot-planner"; "riot-store"; ]
          "riot-build";
      ]
  in
  match plan_build_units workspace (request ()) with
  | Error _ -> Error "expected successful build-unit plan"
  | Ok (_graph, units) ->
      let names = List.map units ~fn:(fun (unit: Build_unit.t) -> (Build_unit.key unit).package) in
      let position name =
        let name = package_name name in
        List.enumerate names
        |> List.find ~fn:(fun (_, current) -> Package_name.equal name current)
        |> Option.map ~fn:(fun (index, _) -> index)
        |> Option.expect ~msg:("missing package in plan: " ^ Package_name.to_string name)
      in
      Test.assert_true (position "std" < position "kernel");
      Test.assert_true (position "kernel" < position "actors");
      Test.assert_true (position "actors" < position "riot-model");
      Test.assert_true (position "riot-model" < position "riot-planner");
      Test.assert_true (position "riot-planner" < position "riot-store");
      Test.assert_true (position "riot-store" < position "riot-build");
      Ok ()

let plan_single_package_includes_only_transitive_closure = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "kernel";
        make_package ~dependencies:[ "kernel" ] "a";
        make_package ~dependencies:[ "a" ] "app";
        make_package ~dependencies:[ "std" ] "unrelated";
      ]
  in
  match plan_build_units workspace (request ~roots:[ package_name "app" ] ()) with
  | Error _ -> Error "expected successful package-root plan"
  | Ok (_graph, units) ->
      Test.assert_equal
        ~expected:(List.map [ "a"; "app"; "kernel"; "std"; ] ~fn:package_name)
        ~actual:(package_names units);
      Ok ()

let plan_multiple_packages_includes_union_of_dependencies = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "kernel";
        make_package ~dependencies:[ "kernel" ] "a";
        make_package ~dependencies:[ "std" ] "b";
        make_package ~dependencies:[ "a" ] "app";
        make_package ~dependencies:[ "b" ] "tool";
        make_package ~dependencies:[ "std" ] "unrelated";
      ]
  in
  match plan_build_units
    workspace
    (request ~roots:(List.map [ "app"; "tool" ] ~fn:package_name) ()) with
  | Error _ -> Error "expected successful multi-package plan"
  | Ok (_graph, units) ->
      Test.assert_equal
        ~expected:(List.map [ "a"; "app"; "b"; "kernel"; "std"; "tool"; ] ~fn:package_name)
        ~actual:(package_names units);
      Ok ()

let plan_unknown_package_reports_available_packages = fun _ctx ->
  let workspace =
    make_workspace [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ]
  in
  match plan_build_units workspace (request ~roots:[ package_name "missing" ] ()) with
  | Ok _ -> Error "expected missing root package"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Test.assert_equal ~expected:[ "root:missing" ] ~actual:(List.map missing ~fn:missing_to_string);
      Ok ()

let plan_multiple_unknown_packages_reports_all_missing_names = fun _ctx ->
  let workspace =
    make_workspace [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ]
  in
  match plan_build_units
    workspace
    (request ~roots:(List.map [ "missing-a"; "app"; "missing-b" ] ~fn:package_name) ()) with
  | Ok _ -> Error "expected missing root packages"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Test.assert_equal
        ~expected:[ "root:missing-a"; "root:missing-b" ]
        ~actual:(
          missing
          |> List.map ~fn:missing_to_string
          |> List.sort ~compare:String.compare
        );
      Ok ()

let plan_reports_missing_dependencies_before_sorting = fun _ctx ->
  let workspace = make_workspace [ make_package ~dependencies:[ "missing-lib" ] "app" ] in
  match plan_build_units workspace (request ()) with
  | Ok _ -> Error "expected missing dependency"
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Test.assert_equal ~expected:[ "app->missing-lib" ] ~actual:(List.map missing ~fn:missing_to_string);
      Ok ()

let plan_runtime_target_does_not_pull_build_dependency_runtime_cycle = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package ~dependencies:[ "core" ] "app";
        make_package ~build_dependencies:[ "builder" ] "core";
        make_package ~dependencies:[ "syntax" ] "builder";
        make_package ~dependencies:[ "core" ] "syntax";
      ]
  in
  match plan_build_units workspace (request ~roots:[ package_name "app" ] ()) with
  | Error _ -> Error "expected runtime roots to ignore build-dependency cycle"
  | Ok (_graph, units) ->
      Test.assert_equal
        ~expected:[ "core:runtime"; "app:runtime" ]
        ~actual:(package_keys units);
      Ok ()

let plan_targeted_runtime_ignores_unrelated_missing_dependencies = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package "std";
        make_package ~dependencies:[ "std" ] "app";
        make_package ~dependencies:[ "missing-lib" ] "unrelated";
      ]
  in
  match plan_build_units workspace (request ~roots:[ package_name "app" ] ()) with
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Error ("expected targeted runtime plan to ignore unrelated missing dependencies, got "
      ^ String.concat ", " (List.map missing ~fn:missing_to_string))
  | Ok (_graph, units) ->
      Test.assert_equal ~expected:(List.map [ "app"; "std" ] ~fn:package_name) ~actual:(package_names units);
      Ok ()

let plan_all_dev_keeps_dependency_packages_runtime_scoped = fun _ctx ->
  let workspace =
    make_workspace
      [
        make_package ~workspace_member:false ~dev_dependencies:[ "propane" ] "std";
        make_package
          ~dependencies:[ "std" ]
          ~binaries:[ test_binary "app_tests" ]
          "app";
      ]
  in
  match plan_build_units
    workspace
    (request ~kind:(Build_unit_graph.Dev default_dev_artifacts) ()) with
  | Error (Build_unit_graph.MissingPackages { missing }) ->
      Error ("expected dependency dev dependencies to stay out of the build plan, got missing: "
      ^ String.concat ", " (List.map missing ~fn:missing_to_string))
  | Ok (_graph, units) ->
      Test.assert_equal
        ~expected:[ "std:runtime"; "app:runtime"; "app:test:app_tests" ]
        ~actual:(package_keys units);
      Ok ()

let tests =
  Test.[
    case
      "plan all runtime returns workspace-like order"
      plan_all_runtime_returns_workspace_like_order;
    case
      "plan single package includes only transitive closure"
      plan_single_package_includes_only_transitive_closure;
    case
      "plan multiple packages includes union of dependencies"
      plan_multiple_packages_includes_union_of_dependencies;
    case
      "plan unknown package reports available packages"
      plan_unknown_package_reports_available_packages;
    case
      "plan multiple unknown packages report all missing names"
      plan_multiple_unknown_packages_reports_all_missing_names;
    case
      "plan reports missing dependencies before sorting"
      plan_reports_missing_dependencies_before_sorting;
    case
      "runtime target does not pull build-dependency runtime cycle"
      plan_runtime_target_does_not_pull_build_dependency_runtime_cycle;
    case
      "targeted runtime ignores unrelated missing dependencies"
      plan_targeted_runtime_ignores_unrelated_missing_dependencies;
    case
      "plan all dev keeps dependency packages runtime scoped"
      plan_all_dev_keeps_dependency_packages_runtime_scoped;
  ]

let name = "riot-planner:workspace-planner-targets"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
