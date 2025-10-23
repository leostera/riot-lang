open Std
open Std.Iter
open Miniriot
module Test = Std.Test

let get_workspace_root () =
  let cwd = Path.v (Sys.getcwd ()) in
  match Fs.exists Path.(cwd / Path.v "tusk.toml") with
  | Ok true -> cwd
  | _ -> Path.(cwd / Path.v "../..")

let rec copy_dir_recursive src dst =
  match Fs.create_dir_all dst with
  | Error e -> Error e
  | Ok () -> (
      match Fs.read_dir src with
      | Error e -> Error e
      | Ok iter ->
          let rec copy_entries () =
            match MutIterator.next iter with
            | None -> Ok ()
            | Some entry -> (
                let entry_name = Path.basename entry in
                let src_path = Path.(src / Path.v entry_name) in
                let dst_path = Path.(dst / Path.v entry_name) in
                let result =
                  match Fs.is_dir src_path with
                  | Ok true -> copy_dir_recursive src_path dst_path
                  | Ok false -> Fs.copy ~src:src_path ~dst:dst_path
                  | Error e -> Error e
                in
                match result with
                | Ok () -> copy_entries ()
                | Error e -> Error e)
          in
          copy_entries ())

let get_fixtures_dir () =
  let cwd = Path.v (Sys.getcwd ()) in
  let local_path = Path.(cwd / Path.v "tests" / Path.v "fixtures") in
  let workspace_path =
    Path.(
      cwd / Path.v "packages" / Path.v "tusk-tests" / Path.v "tests"
      / Path.v "fixtures")
  in
  match Fs.exists local_path with Ok true -> local_path | _ -> workspace_path

let setup_test_workspace test_name fixture_name =
  let workspace_root = get_workspace_root () in
  let test_dir =
    Path.(workspace_root / Path.v "target" / Path.v "test" / Path.v test_name)
  in

  let _ = Fs.remove_dir_all test_dir in
  let _ =
    Fs.create_dir_all test_dir |> Result.expect ~msg:"Failed to create test dir"
  in

  let fixtures_dir = get_fixtures_dir () in
  let fixture_path = Path.(fixtures_dir / Path.v fixture_name) in
  let pkg_dir = Path.(test_dir / Path.v fixture_name) in

  copy_dir_recursive fixture_path pkg_dir
  |> Result.expect ~msg:(format "Failed to copy fixture %s" fixture_name);

  pkg_dir

let load_test_workspace test_dir =
  Tusk_model.Workspace_manager.scan test_dir
  |> Result.expect ~msg:"Failed to load workspace"

let test_simple_library_builds () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir = setup_test_workspace "simple-library" "simple-library" in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found in workspace"
    | package :: _ -> (
        let result =
          Tusk_executor.Package_builder.build ~workspace ~package_graph
            ~toolchain:test_toolchain ~store ~package
        in

        match result.status with
        | Built _ | Cached _ -> Ok ()
        | Failed err ->
            Error
              (format "Build failed: %s"
                 (match err with
                 | PlanningFailed _ -> "planning failed"
                 | ExecutionFailed { message } -> message)))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_package_cache_hit () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir = setup_test_workspace "cache-hit" "simple-library" in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found in workspace"
    | package :: _ -> (
        let first_build =
          Tusk_executor.Package_builder.build ~workspace ~package_graph
            ~toolchain:test_toolchain ~store ~package
        in

        match first_build.status with
        | Built _ | Cached _ -> (
            let second_build =
              Tusk_executor.Package_builder.build ~workspace ~package_graph
                ~toolchain:test_toolchain ~store ~package
            in

            match second_build.status with
            | Cached _ -> Ok ()
            | Built _ -> Error "Expected cache hit, got rebuild"
            | Failed err ->
                Error
                  (format "Second build failed: %s"
                     (match err with
                     | PlanningFailed _ -> "planning"
                     | ExecutionFailed { message } -> message)))
        | Failed err ->
            Error
              (format "First build failed: %s"
                 (match err with
                 | PlanningFailed _ -> "planning"
                 | ExecutionFailed { message } -> message)))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_package_cache_miss_on_source_change () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir = setup_test_workspace "cache-miss" "simple-library" in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found in workspace"
    | package :: _ -> (
        let first_build =
          Tusk_executor.Package_builder.build ~workspace ~package_graph
            ~toolchain:test_toolchain ~store ~package
        in

        match first_build.status with
        | Built _ | Cached _ -> (
            let ml_file =
              Path.(package.path / Path.v "src" / Path.v "lib.ml")
            in
            let _ =
              Fs.write "let x = 100\nlet changed = true" ml_file
              |> Result.expect ~msg:"Failed to modify source"
            in

            let updated_workspace = load_test_workspace test_dir in
            let updated_package = List.hd updated_workspace.packages in
            let updated_package_graph =
              Tusk_planner.Package_graph.create updated_workspace
            in
            let second_build =
              Tusk_executor.Package_builder.build ~workspace:updated_workspace
                ~package_graph:updated_package_graph ~toolchain:test_toolchain
                ~store ~package:updated_package
            in

            match second_build.status with
            | Built _ -> Ok ()
            | Cached _ -> Error "Expected cache miss, got cache hit"
            | Failed err ->
                Error
                  (format "Second build failed: %s"
                     (match err with
                     | PlanningFailed _ -> "planning"
                     | ExecutionFailed { message } -> message)))
        | Failed err ->
            Error
              (format "First build failed: %s"
                 (match err with
                 | PlanningFailed _ -> "planning"
                 | ExecutionFailed { message } -> message)))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_multi_module_builds () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir = setup_test_workspace "multi-module" "multi-module" in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found in workspace"
    | package :: _ -> (
        let result =
          Tusk_executor.Package_builder.build ~workspace ~package_graph
            ~toolchain:test_toolchain ~store ~package
        in

        match result.status with
        | Built _ | Cached _ -> Ok ()
        | Failed err ->
            Error
              (format "Build failed: %s"
                 (match err with
                 | PlanningFailed _ -> "planning failed"
                 | ExecutionFailed { message } -> message)))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_multi_package_workspace () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir =
      setup_test_workspace "multi-package" "multi-package-workspace"
    in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    if List.length workspace.packages <> 2 then
      Error
        (format "Expected 2 packages, got %d" (List.length workspace.packages))
    else
      let build_results =
        List.map
          (fun package ->
            Tusk_executor.Package_builder.build ~workspace ~package_graph
              ~toolchain:test_toolchain ~store ~package)
          workspace.packages
      in

      let all_succeeded =
        List.for_all
          (fun build_result ->
            match build_result.Tusk_executor.Package_builder.status with
            | Built _ | Cached _ -> true
            | Failed _ -> false)
          build_results
      in

      if all_succeeded then Ok ()
      else
        let failed =
          List.filter
            (fun build_result ->
              match build_result.Tusk_executor.Package_builder.status with
              | Failed _ -> true
              | _ -> false)
            build_results
        in
        Error (format "%d packages failed to build" (List.length failed))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_multi_package_dependency_order () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir =
      setup_test_workspace "multi-package-order" "multi-package-workspace"
    in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found"
    | packages -> (
        let lib_b =
          List.find_opt
            (fun (p : Tusk_model.Package.t) -> p.name = "lib-b")
            packages
        in

        match lib_b with
        | None -> Error "lib-b not found in workspace"
        | Some lib_b_pkg -> (
            if List.length lib_b_pkg.dependencies <> 1 then
              Error
                (format "Expected lib-b to have 1 dependency, got %d"
                   (List.length lib_b_pkg.dependencies))
            else
              let lib_a =
                List.find_opt
                  (fun (p : Tusk_model.Package.t) -> p.name = "lib-a")
                  packages
              in
              match lib_a with
              | None -> Error "lib-a not found in workspace"
              | Some lib_a_pkg -> (
                  let lib_a_result =
                    Tusk_executor.Package_builder.build ~workspace
                      ~package_graph ~toolchain:test_toolchain ~store
                      ~package:lib_a_pkg
                  in
                  match lib_a_result.status with
                  | Failed err ->
                      Error
                        (format "lib-a build failed: %s"
                           (match err with
                           | PlanningFailed _ -> "planning failed"
                           | ExecutionFailed { message } -> message))
                  | Built _ | Cached _ -> (
                      let result =
                        Tusk_executor.Package_builder.build ~workspace
                          ~package_graph ~toolchain:test_toolchain ~store
                          ~package:lib_b_pkg
                      in

                      match result.status with
                      | Built _ | Cached _ -> Ok ()
                      | Failed err ->
                          Error
                            (format "Build failed: %s"
                               (match err with
                               | PlanningFailed _ -> "planning failed"
                               | ExecutionFailed { message } -> message))))))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_multi_package_parallel_build () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir =
      setup_test_workspace "multi-package-parallel" "multi-package-workspace"
    in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found"
    | packages -> (
        let lib_a =
          List.find_opt
            (fun (p : Tusk_model.Package.t) -> p.name = "lib-a")
            packages
        in

        match lib_a with
        | None -> Error "lib-a not found"
        | Some lib_a_pkg -> (
            let first_build =
              Tusk_executor.Package_builder.build ~workspace ~package_graph
                ~toolchain:test_toolchain ~store ~package:lib_a_pkg
            in

            match first_build.status with
            | Built _ | Cached _ -> (
                let lib_b =
                  List.find_opt
                    (fun (p : Tusk_model.Package.t) -> p.name = "lib-b")
                    packages
                in

                match lib_b with
                | None -> Error "lib-b not found"
                | Some lib_b_pkg -> (
                    let second_build =
                      Tusk_executor.Package_builder.build ~workspace
                        ~package_graph ~toolchain:test_toolchain ~store
                        ~package:lib_b_pkg
                    in

                    match second_build.status with
                    | Built _ | Cached _ -> Ok ()
                    | Failed err ->
                        Error
                          (format "lib-b build failed: %s"
                             (match err with
                             | PlanningFailed _ -> "planning"
                             | ExecutionFailed { message } -> message))))
            | Failed err ->
                Error
                  (format "lib-a build failed: %s"
                     (match err with
                     | PlanningFailed _ -> "planning"
                     | ExecutionFailed { message } -> message))))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_workspace_without_packages () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir = setup_test_workspace "empty-workspace" "empty-workspace" in
    let workspace = load_test_workspace test_dir in

    if List.length workspace.packages = 0 then Ok ()
    else
      Error
        (format "Expected 0 packages, got %d" (List.length workspace.packages))
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_workspace_with_independent_packages () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir =
      setup_test_workspace "independent-packages" "independent-packages"
    in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    if List.length workspace.packages < 2 then
      Error
        (format "Expected at least 2 packages, got %d"
           (List.length workspace.packages))
    else
      let has_dependencies =
        List.exists
          (fun (pkg : Tusk_model.Package.t) -> List.length pkg.dependencies > 0)
          workspace.packages
      in

      if has_dependencies then Error "Expected no dependencies between packages"
      else
        let build_results =
          List.map
            (fun package ->
              Tusk_executor.Package_builder.build ~workspace ~package_graph
                ~toolchain:test_toolchain ~store ~package)
            workspace.packages
        in

        let all_succeeded =
          List.for_all
            (fun build_result ->
              match build_result.Tusk_executor.Package_builder.status with
              | Built _ | Cached _ -> true
              | Failed _ -> false)
            build_results
        in

        if all_succeeded then Ok () else Error "Some packages failed to build"
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_workspace_with_cycle () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir = setup_test_workspace "cyclic-packages" "cyclic-packages" in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found in workspace"
    | package :: _ -> (
        let package_graph = Tusk_planner.Package_graph.create workspace in
        try
          let _ = Tusk_planner.Package_graph.topological_sort package_graph in
          Error "Expected cycle detection to raise exception"
        with Tusk_planner.Package_graph.Cycle_detected _ -> Ok ())
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let test_workspace_with_path_dependencies () =
  try
    let test_toolchain =
      Tusk_toolchain.init ()
      |> Result.expect ~msg:"Failed to initialize test toolchain"
    in
    let test_dir =
      setup_test_workspace "path-dependencies" "path-dependencies"
    in
    let workspace = load_test_workspace test_dir in
    let store = Tusk_store.Store.create ~workspace in
    let package_graph = Tusk_planner.Package_graph.create workspace in

    match workspace.packages with
    | [] -> Error "No packages found"
    | packages ->
        let has_path_dep =
          List.exists
            (fun (pkg : Tusk_model.Package.t) ->
              List.exists
                (fun (dep : Tusk_model.Package.dependency) ->
                  match dep.source with
                  | Tusk_model.Package.Path _ -> true
                  | _ -> false)
                pkg.dependencies)
            packages
        in

        if not has_path_dep then
          Error "Expected at least one package with path dependency"
        else
          let build_results =
            List.map
              (fun package ->
                Tusk_executor.Package_builder.build ~workspace ~package_graph
                  ~toolchain:test_toolchain ~store ~package)
              packages
          in

          let all_succeeded =
            List.for_all
              (fun build_result ->
                match build_result.Tusk_executor.Package_builder.status with
                | Built _ | Cached _ -> true
                | Failed _ -> false)
              build_results
          in

          if all_succeeded then Ok ()
          else Error "Some packages with path dependencies failed to build"
  with e -> Error (format "Exception in test: %s" (Exception.to_string e))

let tests =
  let open Test in
  [
    case "simple library: builds successfully" test_simple_library_builds;
    case "package cache: hit on second build" test_package_cache_hit;
    case "package cache: miss on source change"
      test_package_cache_miss_on_source_change;
    case "multi-module: builds successfully" test_multi_module_builds;
    case "multi-package: workspace loads all packages"
      test_multi_package_workspace;
    case "multi-package: dependency resolution works"
      test_multi_package_dependency_order;
    case "multi-package: can build dependent packages"
      test_multi_package_parallel_build;
    case "workspace: without packages finishes early"
      test_workspace_without_packages;
    case "workspace: with independent packages builds all"
      test_workspace_with_independent_packages;
    case "workspace: with cycle fails and reports cycle"
      test_workspace_with_cycle;
    case "workspace: with path dependencies builds correctly"
      test_workspace_with_path_dependencies;
  ]

let name = "Tusk Integration Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
