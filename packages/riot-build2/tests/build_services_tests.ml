open Std
open Std.Result.Syntax

module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let dependency_source =
  Riot_model.Package.{
    workspace = true;
    builtin = false;
    path = None;
    source_locator = None;
    ref_ = None;
    version = None;
  }

let package_manifest = fun ?(dependencies = []) name ->
  let name = package name in
  Riot_model.Package.make
    ~name
    ~path:Path.(Path.v "." / Path.v (Riot_model.Package_name.to_string name))
    ~relative_path:(Path.v (Riot_model.Package_name.to_string name))
    ~dependencies
    ()
  |> Riot_model.Package_manifest.from_package

let workspace =
  let dep_name = package "dep" in
  Riot_model.Workspace.make
    ~root:(Path.v ".")
    ~target_dir:(Path.v "_build/riot-build2-build-services-tests")
    ~packages:[
      package_manifest "dep";
      package_manifest
        ~dependencies:[ Riot_model.Package.{ name = dep_name; source = dependency_source } ]
        "app";
    ]
    ()

let config = fun () -> Config.make ~workspace ~parallelism:1 ()

let build_package = fun name ->
  Goal.{
    package = package name;
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
  }

let build_goal = fun name ->
  Goal.BuildPackage (build_package name)

let has_goal_key = fun keys goal ->
  List.any
    keys
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Work_node.GoalKey got -> got = goal
      | _ -> false)

let has_toolchain_key = fun keys target ->
  List.any
    keys
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Work_node.ToolchainReadyKey toolchain -> Riot_model.Target.equal toolchain.target target
      | _ -> false)

let has_package_artifact_key = fun keys build ->
  List.any
    keys
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Work_node.PackageArtifactKey got -> got = build
      | _ -> false)

let key_name = fun __tmp1 ->
  match __tmp1 with
  | Work_node.Intent _ -> "Intent"
  | Package _ -> "Package"
  | Module _ -> "Module"
  | Source _ -> "Source"
  | GoalKey _ -> "GoalKey"
  | ToolchainReadyKey _ -> "ToolchainReadyKey"
  | SourceAnalysisKey _ -> "SourceAnalysisKey"
  | PackageArtifactKey _ -> "PackageArtifactKey"
  | PackageFinalizeKey _ -> "PackageFinalizeKey"
  | ModulePlanKey _ -> "ModulePlanKey"
  | ActionPlanKey _ -> "ActionPlanKey"
  | ActionExecutionKey _ -> "ActionExecutionKey"

let source_package_workspace = fun root ->
  let package_name = package "sourcepkg" in
  let package_path = Path.(root / Path.v "sourcepkg") in
  let source = Path.v "src/sourcepkg.ml" in
  let* () =
    Fs.create_dir_all Path.(package_path / Path.v "src")
    |> Result.map_err ~fn:IO.error_message
  in
  let* () =
    Fs.write "let value = 1\n" Path.(package_path / source)
    |> Result.map_err ~fn:IO.error_message
  in
  let sources =
    Riot_model.Package.{
      src = [ source ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
  in
  let package =
    Riot_model.Package.make
      ~name:package_name
      ~path:package_path
      ~relative_path:(Path.v "sourcepkg")
      ~library:{ path = source }
      ~sources
      ()
    |> Riot_model.Package_manifest.from_package
  in
  Ok (
    Riot_model.Workspace.make
      ~root
      ~target_dir:Path.(root / Path.v "target")
      ~packages:[ package ]
      ()
  )

let test_build_package_plans_package_dependencies_before_execution = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let app_build = build_package "app" in
  let app_goal = Goal.BuildPackage app_build in
  let dep_goal = build_goal "dep" in
  let node = Work_node.goal ~id:(Work_node.Node_id.from_int 1) app_goal in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if has_goal_key keys dep_goal then
        if List.length keys = 2 && has_package_artifact_key keys app_build then
          Ok ()
        else
          Error "expected app build goal to plan manifest package deps and package artifact"
      else
        Error "expected app build goal to plan dep build goal before execution")

let test_build_package_without_package_dependencies_plans_no_dependencies = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let node = Work_node.goal ~id:(Work_node.Node_id.from_int 1) (build_goal "dep") in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if List.length keys = 1 && has_package_artifact_key keys (build_package "dep") then
        Ok ()
      else
        Error "expected package planning to include only package artifact for package without deps")

let test_build_package_does_not_plan_toolchain_readiness = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let goal = build_goal "dep" in
  let target =
    match goal with
    | Goal.BuildPackage build -> build.target
    | _ -> Riot_model.Target.current
  in
  let node = Work_node.goal ~id:(Work_node.Node_id.from_int 1) goal in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if has_toolchain_key keys target then
        Error "expected package-level planning not to add toolchain readiness"
      else
        Ok ())

let test_module_plan_dependencies_are_stable_without_source_analysis_state = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_module_plan_stability"
    (fun root ->
      let* workspace = source_package_workspace root in
      let services =
        Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) ()
      in
      let registry = Work_registry.create () in
      let build = build_package "sourcepkg" in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* first =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
      in
      let* () =
        if List.is_empty first then
          Ok ()
        else
          Error (
            "expected module planning to avoid source analysis during stable dependency planning, got "
            ^ Int.to_string (List.length first)
            ^ ": "
            ^ (List.map first ~fn:key_name |> String.concat ", ")
          )
      in
      let* _ =
        Build_services.execute_node services registry node
        |> Result.map_err ~fn:Error.message
      in
      let* second =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
      in
      if first = second then
        Ok ()
      else
        Error "expected module planning dependencies to ignore source-analysis cache state") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_module_plan_declares_package_dependency_provider_nodes = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let app = build_package "app" in
  let dep = build_goal "dep" in
  let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) app in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if has_goal_key keys dep then
        Ok ()
      else
        Error "expected module planning to depend on declared package provider")

let test_module_plan_cache_hit_skips_dynamic_source_dependencies = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_module_plan_cache"
    (fun root ->
      let* workspace = source_package_workspace root in
      let build = build_package "sourcepkg" in
      let config = Config.make ~workspace ~parallelism:1 () in
      let services = Build_services.create ~config () in
      let registry = Work_registry.create () in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* source_keys =
        match Build_services.execute_node services registry node with
        | Ok (Work_result.RequeueWithDependencies keys) ->
            let source_keys =
              List.filter
                keys
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | Work_node.SourceAnalysisKey _ -> true
                  | _ -> false)
            in
            if List.is_empty source_keys then
              Error "expected cold module planning to request source analysis dependencies"
            else
              Ok source_keys
        | Ok _ -> Error "expected cold module planning to request source analysis dependencies"
        | Error error -> Error (Error.message error)
      in
      let rec execute_sources = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok ()
        | key :: rest -> (
            match Work_registry.find registry key with
            | None -> Error "expected source analysis dependency to be registered"
            | Some source_node ->
                let* _ =
                  Build_services.execute_node services registry source_node
                  |> Result.map_err ~fn:Error.message
                in
                execute_sources rest
          )
      in
      let* () = execute_sources source_keys in
      let* () =
        match Build_services.execute_node services registry node with
        | Ok (Work_result.Complete []) -> Ok ()
        | Ok _ -> Error "expected module plan to complete after source analysis"
        | Error error -> Error (Error.message error)
      in
      let cached_services = Build_services.create ~config () in
      let cached_registry = Work_registry.create () in
      let cached_node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      match Build_services.execute_node cached_services cached_registry cached_node with
      | Ok (Work_result.Complete []) ->
          if List.any source_keys ~fn:(fun key -> Option.is_some (Work_registry.find cached_registry key)) then
            Error "expected module plan cache hit not to register source analysis dependencies"
          else
            Ok ()
      | Ok _ -> Error "expected module plan cache hit to complete without dynamic dependencies"
      | Error error -> Error (Error.message error)) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let action_package = fun root ->
  let name = package "action-pkg" in
  Riot_model.Package.make
    ~name
    ~path:Path.(root / Path.v "action-pkg")
    ~relative_path:(Path.v "action-pkg")
    ()

let action_execution = fun root ~actions ~outs ->
  let target = Riot_model.Target.current in
  let package = action_package root in
  let toolchain =
    Riot_toolchain.from_config_for_target
      ~config:(Riot_model.Toolchain_config.from_root ~root)
      ~target
  in
  let graph = Riot_planner.Action_graph.create () in
  let spec =
    Riot_planner.Action_node.make
      ~actions
      ~outs
      ~srcs:[]
      ~package
      ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let action = Riot_planner.Action_graph.add_node graph spec in
  Action_execution.make
    ~package:package.name
    ~profile:Riot_model.Profile.debug
    ~target
    ~action
    ~dependencies:[]
    ~sandbox_dir:Path.(root / Path.v "sandbox")

let write_action_execution = fun root ->
  let output = Path.v "out.txt" in
  action_execution
    root
    ~actions:[ Riot_planner.Action.WriteFile { destination = output; content = "hello" } ]
    ~outs:[ output ]

let copy_file_action_execution = fun root ->
  let source = Path.v "data.txt" in
  let destination = Path.v "copied.txt" in
  action_execution
    root
    ~actions:[ Riot_planner.Action.CopyFile { source; destination } ]
    ~outs:[ destination ]

let compile_action_execution = fun root ->
  action_execution
    root
    ~actions:[
      Riot_planner.Action.CompileInterface {
        source = Path.v "example.mli";
        outputs = [ Path.v "example.cmi" ];
        includes = [];
        flags = [];
      };
    ]
    ~outs:[ Path.v "example.cmi" ]

let test_action_execution_plans_toolchain_readiness_for_compiler_action = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_planned_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let action = compile_action_execution root in
      let node = Work_node.action_execution ~id:(Work_node.Node_id.from_int 1) action in
      Build_services.plan_dependencies services registry node
      |> Result.map_err ~fn:Error.message
      |> Result.and_then
        ~fn:(fun keys ->
          if has_toolchain_key keys action.ref_.target then
            Ok ()
          else
            Error "expected compiler action planning to include toolchain readiness")) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_action_execution_does_not_plan_toolchain_for_noncompiler_action = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_no_planned_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let action = write_action_execution root in
      let node = Work_node.action_execution ~id:(Work_node.Node_id.from_int 1) action in
      Build_services.plan_dependencies services registry node
      |> Result.map_err ~fn:Error.message
      |> Result.and_then
        ~fn:(fun keys ->
          if has_toolchain_key keys action.ref_.target then
            Error "expected noncompiler action planning not to include toolchain readiness"
          else
            Ok ())) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_uncached_noncompiler_action_executes_without_toolchain_readiness = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_no_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let executor = Action_executor.create ~store ~toolchains () in
      let action = write_action_execution root in
      match Action_executor.execute executor action with
      | Ok (Work_result.Complete []) ->
          let result =
            match Action_executor.find_result executor action.ref_ with
            | Some { Action_execution.status = Action_execution.Executed _; _ } -> Ok ()
            | Some _ -> Error "expected uncached noncompiler action to execute"
            | None -> Error "expected uncached noncompiler action result"
          in
          result
      | Ok _ -> Error "expected uncached noncompiler action not to request dependencies"
      | Error error -> Error (Error.message error)) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_uncached_action_reads_concrete_package_sources_without_sandbox_copy = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_package_source"
    (fun root ->
      let package = action_package root in
      let* () =
        Fs.create_dir_all package.path
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "package-source\n" Path.(package.path / Path.v "data.txt")
        |> Result.map_err ~fn:IO.error_message
      in
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let executor = Action_executor.create ~store ~toolchains () in
      let action = copy_file_action_execution root in
      match Action_executor.execute executor action with
      | Ok (Work_result.Complete []) -> (
          match Fs.read Path.(action.sandbox_dir / Path.v "copied.txt") with
          | Ok "package-source\n" -> Ok ()
          | Ok _ -> Error "expected copied package source content"
          | Error error -> Error ("expected copied package source: " ^ IO.error_message error)
        )
      | Ok _ -> Error "expected package-source copy action not to request dependencies"
      | Error error -> Error (Error.message error)) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_uncached_compiler_action_requests_toolchain_readiness_at_execution = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let executor = Action_executor.create ~store ~toolchains () in
      let action = compile_action_execution root in
      match Action_executor.execute executor action with
      | Ok (Work_result.RequeueWithDependencies [ Work_node.ToolchainReadyKey toolchain ]) when Riot_model.Target.equal
        toolchain.target
        action.ref_.target -> Ok ()
      | Ok _ -> Error "expected uncached action to request toolchain readiness"
      | Error error -> Error (Error.message error)) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let tests =
  Test.[
    case
      "build package plans package dependencies before execution"
      test_build_package_plans_package_dependencies_before_execution;
    case
      "build package without package dependencies plans no dependencies"
      test_build_package_without_package_dependencies_plans_no_dependencies;
    case
      "build package does not plan toolchain readiness"
      test_build_package_does_not_plan_toolchain_readiness;
    case
      "module plan dependencies are stable without source analysis state"
      test_module_plan_dependencies_are_stable_without_source_analysis_state;
    case
      "module plan declares package dependency provider nodes"
      test_module_plan_declares_package_dependency_provider_nodes;
    case
      "module plan cache hit skips dynamic source dependencies"
      test_module_plan_cache_hit_skips_dynamic_source_dependencies;
    case
      "action execution plans toolchain readiness for compiler action"
      test_action_execution_plans_toolchain_readiness_for_compiler_action;
    case
      "action execution does not plan toolchain for noncompiler action"
      test_action_execution_does_not_plan_toolchain_for_noncompiler_action;
    case
      "uncached noncompiler action executes without toolchain readiness"
      test_uncached_noncompiler_action_executes_without_toolchain_readiness;
    case
      "uncached action reads concrete package sources without sandbox copy"
      test_uncached_action_reads_concrete_package_sources_without_sandbox_copy;
    case
      "uncached compiler action requests toolchain readiness at execution"
      test_uncached_compiler_action_requests_toolchain_readiness_at_execution;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_build_services_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
