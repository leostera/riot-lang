open Std
module Test = Std.Test
module Workspace_planner = Riot_planner.Workspace_planner
module Package = Riot_model.Package
module Workspace = Riot_model.Workspace

let dependency = fun name ->
  Package.{
    name;
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

let make_package = fun ?(dependencies = []) ?(dev_dependencies = []) ?(build_dependencies = []) name ->
  Package.make
    ~name
    ~path:(Path.v ("packages/" ^ name))
    ~relative_path:(Path.v ("packages/" ^ name))
    ~dependencies:(List.map dependencies ~fn:dependency)
    ~dev_dependencies:(List.map dev_dependencies ~fn:dependency)
    ~build_dependencies:(List.map build_dependencies ~fn:dependency)
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_workspace = fun packages ->
  Workspace.make_realized
    ~root:(Path.v "/tmp/workspace_planner_target_tests")
    ~packages
    ()

let plan_workspace = fun workspace target scope ->
  Workspace_planner.plan_workspace ~workspace ~target ~scope ~load_errors:[]

let package_names = fun plan ->
  Workspace_planner.packages_in_plan plan |> List.map ~fn:(fun (pkg: Package.t) -> pkg.name)

let package_keys = fun plan ->
  Riot_planner.Package_graph.topological_sort plan.Workspace_planner.package_graph
  |> List.map ~fn:Riot_planner.Package_graph.get_key
  |> List.map ~fn:Riot_model.Package.key_to_string

let plan_all_runtime_returns_workspace_like_order = fun _ctx ->
  let workspace = make_workspace
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "std"; "kernel" ] "actors";
      make_package ~dependencies:[ "std"; "actors" ] "riot-model";
      make_package ~dependencies:[ "std"; "riot-model" ] "riot-planner";
      make_package ~dependencies:[ "std"; "riot-model"; "riot-planner" ] "riot-store";
      make_package ~dependencies:[ "std"; "riot-model"; "riot-planner"; "riot-store" ] "riot-build";
    ] in
  match plan_workspace workspace All Runtime with
  | Error _ -> Error "expected successful workspace plan"
  | Ok plan ->
      let names = package_names plan in
      let position name =
        List.enumerate names
        |> List.find ~fn:(fun (_, current) -> String.equal name current)
        |> Option.map ~fn:(fun (index, _) -> index)
        |> Option.expect ~msg:("missing package in plan: " ^ name)
      in
      Test.assert_true (position "std" < position "kernel");
      Test.assert_true (position "kernel" < position "actors");
      Test.assert_true (position "actors" < position "riot-model");
      Test.assert_true (position "riot-model" < position "riot-planner");
      Test.assert_true (position "riot-planner" < position "riot-store");
      Test.assert_true (position "riot-store" < position "riot-build");
      Ok ()

let plan_single_package_includes_only_transitive_closure = fun _ctx ->
  let workspace = make_workspace
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "kernel" ] "a";
      make_package ~dependencies:[ "a" ] "app";
      make_package ~dependencies:[ "std" ] "unrelated";
    ] in
  match plan_workspace workspace (Package "app") Runtime with
  | Error _ -> Error "expected successful package-target plan"
  | Ok plan ->
      let names = package_names plan |> List.unique ~compare:String.compare in
      Test.assert_equal ~expected:[ "a"; "app"; "kernel"; "std" ] ~actual:names;
      Ok ()

let plan_multiple_packages_includes_union_of_dependencies = fun _ctx ->
  let workspace = make_workspace
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "kernel" ] "a";
      make_package ~dependencies:[ "std" ] "b";
      make_package ~dependencies:[ "a" ] "app";
      make_package ~dependencies:[ "b" ] "tool";
      make_package ~dependencies:[ "std" ] "unrelated";
    ] in
  match plan_workspace workspace (Packages [ "app"; "tool" ]) Runtime with
  | Error _ -> Error "expected successful multi-package plan"
  | Ok plan ->
      let names = package_names plan |> List.unique ~compare:String.compare in
      Test.assert_equal ~expected:[ "a"; "app"; "b"; "kernel"; "std"; "tool" ] ~actual:names;
      Ok ()

let plan_unknown_package_reports_available_packages = fun _ctx ->
  let workspace = make_workspace [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ] in
  match plan_workspace workspace (Package "missing") Runtime with
  | Ok _ ->
      Error "expected PackageNotFound"
  | Error (PackageNotFound { name; available }) ->
      Test.assert_equal ~expected:"missing" ~actual:name;
      Test.assert_equal ~expected:[ "app"; "std" ] ~actual:(List.sort available ~compare:String.compare);
      Ok ()
  | Error _ ->
      Error "expected PackageNotFound"

let plan_multiple_unknown_packages_reports_all_missing_names = fun _ctx ->
  let workspace = make_workspace [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ] in
  match plan_workspace workspace (Packages [ "missing-a"; "app"; "missing-b" ]) Runtime with
  | Ok _ ->
      Error "expected PackagesNotFound"
  | Error (PackagesNotFound { names; available }) ->
      Test.assert_equal ~expected:[ "missing-a"; "missing-b" ] ~actual:names;
      Test.assert_equal ~expected:[ "app"; "std" ] ~actual:(List.sort available ~compare:String.compare);
      Ok ()
  | Error _ ->
      Error "expected PackagesNotFound"

let plan_reports_missing_dependencies_before_sorting = fun _ctx ->
  let workspace = make_workspace [ make_package ~dependencies:[ "missing-lib" ] "app" ] in
  match plan_workspace workspace All Runtime with
  | Ok _ ->
      Error "expected MissingDependencies"
  | Error (MissingDependencies { missing }) ->
      let entries =
        List.map
          missing
          ~fn:(fun (item: Riot_planner.Package_graph.missing_dependency) -> item.package ^ "->" ^ item.dependency)
      in
      Test.assert_equal ~expected:[ "app->missing-lib" ] ~actual:entries;
      Ok ()
  | Error _ ->
      Error "expected MissingDependencies"

let plan_runtime_target_does_not_pull_build_dependency_runtime_cycle = fun _ctx ->
  let workspace = make_workspace
    [
      make_package ~dependencies:[ "core" ] "app";
      make_package ~build_dependencies:[ "builder" ] "core";
      make_package ~dependencies:[ "syntax" ] "builder";
      make_package ~dependencies:[ "core" ] "syntax";
    ] in
  match plan_workspace workspace (Package "app") Runtime with
  | Error (CycleDetected { cycle }) ->
      Error ("expected runtime target planning to avoid build-dependency cycle, got cycle: "
      ^ String.concat " -> " cycle)
  | Error _ ->
      Error "expected successful package-target plan"
  | Ok plan ->
      Test.assert_equal
        ~expected:[ "core:build"; "core:runtime"; "app:runtime" ]
        ~actual:(package_keys plan);
      Ok ()

let plan_targeted_runtime_ignores_unrelated_missing_dependencies = fun _ctx ->
  let workspace = make_workspace
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "app";
      make_package ~dependencies:[ "missing-lib" ] "unrelated";
    ] in
  match plan_workspace workspace (Package "app") Runtime with
  | Error err ->
      Error ("expected targeted runtime plan to ignore unrelated missing dependencies, got " ^ (
        match err with
        | PackageNotFound { name; _ } -> "PackageNotFound(" ^ name ^ ")"
        | PackagesNotFound { names; _ } -> "PackagesNotFound(" ^ String.concat "," names ^ ")"
        | CycleDetected { cycle } -> "CycleDetected(" ^ String.concat "->" cycle ^ ")"
        | MissingDependencies { missing } -> "MissingDependencies("
          ^ String.concat
              ","
              (List.map
                missing
                ~fn:(fun (item: Riot_planner.Package_graph.missing_dependency) -> item.package ^ "->" ^ item.dependency))
          ^ ")"
        | PackageLoadFailed _ -> "PackageLoadFailed"
      ))
  | Ok plan ->
      let names = package_names plan |> List.unique ~compare:String.compare in
      Test.assert_equal ~expected:[ "app"; "std" ] ~actual:names;
      Ok ()

let tests =
  Test.[
    case "plan all runtime returns workspace-like order" plan_all_runtime_returns_workspace_like_order;
    case "plan single package includes only transitive closure" plan_single_package_includes_only_transitive_closure;
    case "plan multiple packages includes union of dependencies" plan_multiple_packages_includes_union_of_dependencies;
    case "plan unknown package reports available packages" plan_unknown_package_reports_available_packages;
    case "plan multiple unknown packages report all missing names" plan_multiple_unknown_packages_reports_all_missing_names;
    case "plan reports missing dependencies before sorting" plan_reports_missing_dependencies_before_sorting;
    case
      "runtime target does not pull build-dependency runtime cycle"
      plan_runtime_target_does_not_pull_build_dependency_runtime_cycle;
    case
      "targeted runtime ignores unrelated missing dependencies"
      plan_targeted_runtime_ignores_unrelated_missing_dependencies;
  ]

let name = "riot-planner:workspace-planner-targets"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
