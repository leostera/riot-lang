open Std
open Riot_build
open Std.Collections
open Riot_model

module Action_scheduler = Riot_build.Internal.Action_scheduler
module Sandbox = Riot_build.Internal.Sandbox
module Test = Std.Test
module G = Std.Graph.SimpleGraph

let test_toolchain =
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let make_registry = fun root ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~riot_home:Path.(root / Path.v ".riot")
      ~registry_name:"pkgs.ml"
      ()
    |> Result.expect ~msg:"registry cache init failed"
  in
  Pkgs_ml.Registry.in_memory ~cache ~packages:[] ()

let clone_workspace_with_target = fun (workspace: Riot_model.Workspace.t) ~target_dir ->
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:workspace.packages
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~target_dir:(Path.to_string target_dir)
    ()

let load_repo_workspace = fun () ->
  let manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan manager (Path.v ".") with
  | Error err ->
      Error ("workspace scan failed: " ^ Riot_model.Workspace_manager.scan_error_message err)
  | Ok (workspace_manifest, errors) ->
      if List.is_empty errors then
        Riot_deps.ensure_workspace
          ~workspace_manager:manager
          ~mode:Riot_deps.Dep_solver.Refresh
          ~registry:(make_registry (Path.v "."))
          ~workspace:workspace_manifest
          ()
        |> Result.map_err ~fn:Riot_model.Pm_error.message
      else
        Error ("workspace scan produced load errors: "
        ^ String.concat "; " (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))

let find_package_by_name = fun (workspace: Riot_model.Workspace.t) name ->
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Runtime workspace
  |> List.find
    ~fn:(fun (pkg: Riot_model.Package.t) -> Package_name.equal pkg.name (package_name name))

let plan_kernel_runtime_graphs = fun ~workspace ~store ~build_ctx ->
  match find_package_by_name workspace "kernel" with
  | None -> Error "kernel package not found in workspace"
  | Some package ->
      let key =
        ({
          package = package.name;
          artifact = Riot_planner.Build_unit.Library;
          target = Riot_model.Target.host ();
          profile = Riot_model.Profile.debug;
        }:Riot_planner.Build_unit.key)
      in
      let unit =
        Riot_planner.Build_unit.from_artifact
          ~package
          ~artifact:key.artifact
          ~target:key.target
          ~profile:key.profile
      in
      match Riot_planner.Package_planner.plan_build_unit
        ~on_source_analyzed:(fun _ -> ())
        ~workspace
        ~toolchain:test_toolchain
        ~store
        ~unit
        ~depset:[]
        ~build_ctx with
      | Error err -> Error (Riot_planner.Planning_error.to_string err)
      | Ok (Riot_planner.Package_planner.Planned { action_graph; depset; _ }) ->
          Ok (package, action_graph, depset)
      | Ok (Cached _) -> Error "expected kernel runtime plan to return Planned"

let action_label = fun (node: Riot_planner.Action_node.t) ->
  let actions =
    (Riot_planner.Action_node.value node).actions
    |> List.map ~fn:Riot_planner.Action.to_string
    |> String.concat " ; "
  in
  G.Node_id.to_string (Riot_planner.Action_node.id node) ^ " => " ^ actions

let summarize_execution_failures = fun ~sandbox_dir result ->
  let failures =
    result.Action_scheduler.completed_actions
    |> List.filter_map
      ~fn:(fun completed_action ->
        let action = action_label completed_action.Action_scheduler.node in
        match completed_action.result.status with
        | Action_scheduler.Failed (
          Action_scheduler.ExecutionFailed { message }
        ) ->
            Some (action ^ "\n" ^ message)
        | Action_scheduler.Failed (
          Action_scheduler.OutputsNotCreated { missing }
        ) ->
            Some (action
            ^ "\nmissing outputs: "
            ^ String.concat ", " (List.map missing ~fn:Path.to_string))
        | Action_scheduler.Failed (
          Action_scheduler.DependenciesFailed { failed }
        ) ->
            Some (action
            ^ "\nfailed deps: "
            ^ String.concat ", " (List.map failed ~fn:G.Node_id.to_string))
        | Action_scheduler.Cached _
        | Action_scheduler.Executed _
        | Action_scheduler.Skipped -> None)
  in
  "sandbox: " ^ Path.to_string sandbox_dir ^ "\nfailures:\n" ^ String.concat "\n\n" failures

let execute_kernel_runtime_graph = fun ~concurrency ->
  match Fs.with_tempdir
    ~prefix:"riot_executor_kernel_runtime"
    (fun tempdir ->
      match load_repo_workspace () with
      | Error _ as err -> err
      | Ok repo_workspace ->
          let workspace =
            clone_workspace_with_target repo_workspace ~target_dir:Path.(tempdir / Path.v "target")
          in
          let store = Riot_store.Store.create ~workspace in
          let session_id = Riot_model.Session_id.make () in
          let build_ctx =
            Riot_model.Build_ctx.make ~session_id ~profile:Riot_model.Profile.debug ()
          in
          match plan_kernel_runtime_graphs ~workspace ~store ~build_ctx with
          | Error _ as err -> err
          | Ok (package, action_graph, depset) ->
              let inputs =
                List.concat [ package.sources.src; package.sources.native; package.sources.tests ]
              in
              let sandbox =
                Sandbox.create
                  ~workspace
                  ~profile:"debug"
                  ~target:(Riot_model.Riot_dirs.host_target ())
                  ()
                  ~package_name:package.name
              in
              let _ =
                Sandbox.prepare ~sandbox ~package ~inputs ~depset ~store
                |> Result.expect ~msg:"sandbox prepare should succeed"
              in
              let sandbox_dir = Sandbox.get_dir sandbox in
              let result =
                Action_scheduler.run
                  ~action_graph
                  ~sandbox
                  ~store
                  ~session_id
                  ~build_target:Riot_model.Target.current
                  test_toolchain
                  ~concurrency
              in
              let summary = summarize_execution_failures ~sandbox_dir result in
              Sandbox.cleanup sandbox;
              let failures =
                result.Action_scheduler.completed_actions
                |> List.filter
                  ~fn:(fun completed_action ->
                    match completed_action.Action_scheduler.result.status with
                    | Action_scheduler.Failed _ -> true
                    | Action_scheduler.Cached _
                    | Action_scheduler.Executed _
                    | Action_scheduler.Skipped -> false)
              in
              if List.is_empty failures then
                Ok ()
              else
                Error summary) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_kernel_runtime_graph_executes_serially = fun _ctx ->
  execute_kernel_runtime_graph
    ~concurrency:1

let test_kernel_runtime_graph_executes_in_parallel = fun _ctx ->
  execute_kernel_runtime_graph
    ~concurrency:4

let tests = let open Test in
[
  case
    ~size:Large
    "kernel runtime graph executes serially"
    test_kernel_runtime_graph_executes_serially;
  case
    ~size:Large
    "kernel runtime graph executes in parallel"
    test_kernel_runtime_graph_executes_in_parallel;
]

let name = "riot-build:kernel-runtime-tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
