open Std
open Std.Collections
module Test = Std.Test
module G = Std.Graph.SimpleGraph

let test_toolchain =
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

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
      Error ("workspace scan failed: " ^ err)
  | Ok (workspace, errors) ->
      if List.is_empty errors then
        Ok workspace
      else
        Error
          ("workspace scan produced load errors: "
          ^ String.concat
              "; "
              (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))

let find_package_by_name = fun (workspace: Riot_model.Workspace.t) name ->
  List.find workspace.packages ~fn:(fun (pkg: Riot_model.Package.t) -> String.equal pkg.name name)

let plan_graph_package = fun ~workspace ~store ~package_graph ~package_key ~build_ctx ->
  match Riot_planner.Package_graph.get_node_by_key package_graph package_key with
  | None ->
      Error ("package graph node not found: " ^ Riot_model.Package.key_to_string package_key)
  | Some node ->
      let package = Riot_planner.Package_graph.get_package node.value in
      Riot_planner.Package_planner.plan_package
        ~workspace
        ~toolchain:test_toolchain
        ~store
        ~package_graph
        ~package_key
        ~package
        ~build_ctx
      |> Result.map_err ~fn:Riot_planner.Planning_error.to_string

let plan_kernel_runtime_graphs = fun ~workspace ~store ~build_ctx ->
  match find_package_by_name workspace "kernel" with
  | None ->
      Error "kernel package not found in workspace"
  | Some package ->
      let package_graph =
        Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime
          workspace
        |> Result.expect ~msg:"package graph should build"
      in
      let build_key =
        Riot_planner.Package_graph.package_key
          ~package_name:package.name
          Riot_planner.Package_graph.Build
      in
      let runtime_key =
        Riot_planner.Package_graph.package_key
          ~package_name:package.name
          Riot_planner.Package_graph.Runtime
      in
      match plan_graph_package ~workspace ~store ~package_graph ~package_key:build_key ~build_ctx with
      | Error err ->
          Error ("kernel build-scope plan failed: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; hash; _ }) ->
          let _ =
            Riot_planner.Package_graph.mark_planned
              package_graph
              build_key
              ~module_graph
              ~action_graph
              ~hash
          in
          (
            match plan_graph_package ~workspace ~store ~package_graph ~package_key:runtime_key ~build_ctx with
            | Error err ->
                Error ("kernel runtime plan failed: " ^ err)
            | Ok (Riot_planner.Package_planner.Planned { action_graph; depset; _ }) ->
                Ok (package, action_graph, depset)
            | Ok _ ->
                Error "expected kernel runtime plan to return Planned"
          )
      | Ok _ ->
          Error "expected kernel build-scope plan to return Planned"

let action_label = fun (node: Riot_planner.Action_node.t) ->
  let actions =
    node.value.actions
    |> List.map ~fn:Riot_planner.Action.to_string
    |> String.concat " ; "
  in
  G.Node_id.to_string node.id ^ " => " ^ actions

let summarize_execution_failures = fun ~(action_graph: Riot_planner.Action_graph.t) ~sandbox_dir result ->
  let nodes_by_id =
    Riot_planner.Action_graph.nodes action_graph
    |> List.fold_left
         ~acc:(HashMap.create ())
         ~fn:(fun acc (node: Riot_planner.Action_node.t) ->
           let _ = HashMap.insert acc ~key:node.id ~value:node in
           acc)
  in
  let failures =
    HashMap.to_list result.Riot_executor.Action_executor.completed
    |> List.filter_map ~fn:(fun (node_id, execution_result) ->
         match execution_result.Riot_executor.Action_executor.status with
         | Riot_executor.Action_executor.Failed
             (Riot_executor.Action_executor.ExecutionFailed { message }) ->
             let action =
               match HashMap.get nodes_by_id ~key:node_id with
               | Some node -> action_label node
               | None -> G.Node_id.to_string node_id
             in
             Some (action ^ "\n" ^ message)
         | Riot_executor.Action_executor.Failed
             (Riot_executor.Action_executor.OutputsNotCreated { missing }) ->
             let action =
               match HashMap.get nodes_by_id ~key:node_id with
               | Some node -> action_label node
               | None -> G.Node_id.to_string node_id
             in
             Some
               (action
               ^ "\nmissing outputs: "
               ^ String.concat ", " (List.map missing ~fn:Path.to_string))
         | Riot_executor.Action_executor.Failed
             (Riot_executor.Action_executor.DependenciesFailed { failed }) ->
             let action =
               match HashMap.get nodes_by_id ~key:node_id with
               | Some node -> action_label node
               | None -> G.Node_id.to_string node_id
             in
             Some
               (action
               ^ "\nfailed deps: "
               ^ String.concat ", " (List.map failed ~fn:G.Node_id.to_string))
         | Riot_executor.Action_executor.Cached _
         | Riot_executor.Action_executor.Executed
         | Riot_executor.Action_executor.Skipped ->
             None)
  in
  "sandbox: "
  ^ Path.to_string sandbox_dir
  ^ "\nfailures:\n"
  ^ String.concat "\n\n" failures

let execute_kernel_runtime_graph = fun ~concurrency ->
  match
    Fs.with_tempdir ~prefix:"riot_executor_kernel_runtime"
      (fun tempdir ->
        match load_repo_workspace () with
        | Error _ as err ->
            err
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
            | Error _ as err ->
                err
            | Ok (package, action_graph, depset) ->
                let inputs =
                  List.concat
                    [ package.sources.src; package.sources.native; package.sources.tests ]
                in
                let sandbox =
                  Riot_executor.Sandbox.create
                    ~workspace
                    ~profile:"debug"
                    ~target:(Riot_model.Riot_dirs.host_target ())
                    ()
                    ~package_name:package.name
                in
                Riot_executor.Sandbox.prepare ~sandbox ~package ~inputs ~depset ~store;
                let sandbox_dir = Riot_executor.Sandbox.get_dir sandbox in
                let result =
                  Riot_executor.Action_executor.execute
                    ~action_graph
                    ~sandbox
                    ~store
                    ~session_id
                    test_toolchain
                    ~concurrency
                in
                let summary = summarize_execution_failures ~action_graph ~sandbox_dir result in
                Riot_executor.Sandbox.cleanup sandbox;
                let failures =
                  HashMap.to_list result.Riot_executor.Action_executor.completed
                  |> List.filter ~fn:(fun (_, execution_result) ->
                       match execution_result.Riot_executor.Action_executor.status with
                       | Riot_executor.Action_executor.Failed _ -> true
                       | Riot_executor.Action_executor.Cached _
                       | Riot_executor.Action_executor.Executed
                       | Riot_executor.Action_executor.Skipped -> false)
                in
                if List.is_empty failures then
                  Ok ()
                else
                  Error summary)
  with
  | Ok result ->
      result
  | Error err ->
      Error ("tempdir failed: " ^ IO.error_message err)

let test_kernel_runtime_graph_executes_serially = fun _ctx ->
  execute_kernel_runtime_graph ~concurrency:1

let test_kernel_runtime_graph_executes_in_parallel = fun _ctx ->
  execute_kernel_runtime_graph ~concurrency:4

let tests =
  let open Test in
  [
    case ~size:Large "kernel runtime graph executes serially" test_kernel_runtime_graph_executes_serially;
    case ~size:Large "kernel runtime graph executes in parallel" test_kernel_runtime_graph_executes_in_parallel;
  ]

let name = "riot-executor:kernel-runtime-tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
