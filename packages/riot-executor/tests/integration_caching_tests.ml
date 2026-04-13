open Std
open Std.Collections
module Test = Std.Test

let test_toolchain = fun () ->
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let make_test_build_ctx = fun () ->
  let session_id = Riot_model.Session_id.make () in
  Riot_model.Build_ctx.make ~session_id ~profile:Riot_model.Profile.debug ()

let make_workspace = fun root ->
  Riot_model.Workspace.{
    name = None;
    root;
    target_dir_root =
      Path.(root / Path.v "target");
    packages = [];
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    profile_overrides = [];
  }

let workspace_dependency = fun name ->
  Riot_model.Package.{
    name;
    source = {
      workspace = true;
      builtin = false;
      path = None;
      source_locator = None;
      ref_ = None;
      version = None;
    };
  }

let make_package = fun ~root ~name ->
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Riot_model.Package.make
    ~name
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_library_package = fun ~root ~name ?interface ?(dependencies = []) implementation ->
  let path = Path.(root / Path.v "packages" / Path.v name) in
  let src_dir = Path.(path / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"failed to create src dir" in
  let impl_rel = Path.v ("src/" ^ name ^ ".ml") in
  let impl_path = Path.(path / impl_rel) in
  let _ = Fs.write implementation impl_path |> Result.expect ~msg:"failed to write impl" in
  let srcs =
    match interface with
    | None ->
        [ impl_rel ]
    | Some signature ->
        let intf_rel = Path.v ("src/" ^ name ^ ".mli") in
        let intf_path = Path.(path / intf_rel) in
        let _ = Fs.write signature intf_path |> Result.expect ~msg:"failed to write interface" in
        [ intf_rel; impl_rel ]
  in
  Riot_model.Package.make
    ~name
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~dependencies:(List.map dependencies ~fn:workspace_dependency)
    ~library:{ path = impl_rel }
    ~sources:{
      src = srcs;
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let make_graph_with_write = fun ~package ~content ->
  let graph = Riot_planner.Action_graph.create () in
  let spec =
    Riot_planner.Action_node.make
      ~actions:[ Riot_planner.Action.WriteFile {
        destination = Path.v "out.txt";
        content
      }; ]
      ~outs:[ Path.v "out.txt" ]
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let node = Riot_planner.Action_graph.add_node graph spec in
  (graph, node)

let node_id = fun (node: Riot_planner.Action_node.t) -> node.id

let execute_graph = fun ~workspace ~store ~package ~graph ->
  let sandbox = Riot_executor.Sandbox.create ~workspace () ~package_name:package.Riot_model.Package.name in
  let result = Riot_executor.Action_executor.execute
    ~action_graph:graph
    ~sandbox
    ~store
    ~session_id:(Riot_model.Session_id.make ())
    (test_toolchain ())
    ~concurrency:1
  in
  let output = Path.(Riot_executor.Sandbox.get_dir sandbox / Path.v "out.txt") in
  let output_content = Fs.read_to_string output in
  let output_exists = Fs.exists output |> Result.unwrap_or ~default:false in
  let sandbox_dir = Riot_executor.Sandbox.get_dir sandbox in
  let _ = Riot_executor.Sandbox.cleanup sandbox in
  (result, output_exists, output_content, sandbox_dir)

let build_package = fun ~workspace ~store ~package ~package_graph ->
  Riot_executor.Package_builder.build
    ~workspace
    ~toolchain:(test_toolchain ())
    ~store
    ~build_ctx:(make_test_build_ctx ())
    ~package_graph
    ~package_key:(Riot_planner.Package_graph.package_key
      ~package_name:package.Riot_model.Package.name
      Riot_planner.Package_graph.Runtime)
    ~package

let execute_planned_package = fun ~workspace ~store ~package ~package_graph ->
  match Riot_planner.Package_planner.plan_package
    ~workspace
    ~toolchain:(test_toolchain ())
    ~store
    ~package_graph
    ~package_key:(Riot_planner.Package_graph.package_key
      ~package_name:package.Riot_model.Package.name
      Riot_planner.Package_graph.Runtime)
    ~package
    ~build_ctx:(make_test_build_ctx ()) with
  | Error err ->
      Error ("planning failed: " ^ Riot_planner.Planning_error.to_string err)
  | Ok (Riot_planner.Package_planner.MissingDependencies _)
  | Ok (Riot_planner.Package_planner.FailedDependencies _) ->
      Error "expected dependent package to be plannable"
  | Ok (Riot_planner.Package_planner.Cached _) ->
      Error "expected direct action execution path to replan package"
  | Ok (Riot_planner.Package_planner.Planned { action_graph; depset; _ }) ->
      let sandbox = Riot_executor.Sandbox.create ~workspace () ~package_name:package.Riot_model.Package.name in
      Riot_executor.Sandbox.prepare
        ~sandbox
        ~package
        ~inputs:(package.Riot_model.Package.sources.src @ package.Riot_model.Package.sources.native @ package.Riot_model.Package.sources.tests)
        ~depset
        ~store;
      let result = Riot_executor.Action_executor.execute
        ~action_graph
        ~sandbox
        ~store
        ~session_id:(Riot_model.Session_id.make ())
        (test_toolchain ())
        ~concurrency:1 in
      let _ = Riot_executor.Sandbox.cleanup sandbox in
      Ok result

let test_execute_reuses_cache_for_equivalent_graph = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"integration_cache_equivalent"
      (fun tmpdir ->
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let graph1, node1 = make_graph_with_write ~package ~content:"cached output" in
        let result1, exists1, content1, _sandbox1 = execute_graph ~workspace ~store ~package ~graph:graph1 in
        let graph2, node2 = make_graph_with_write ~package ~content:"cached output" in
        let result2, exists2, content2, _sandbox2 = execute_graph ~workspace ~store ~package ~graph:graph2 in
        match
          ( HashMap.get result1.Riot_executor.Action_executor.completed ~key:(node_id node1),
            HashMap.get result2.Riot_executor.Action_executor.completed ~key:(node_id node2),
            content1,
            content2 )
        with
        | Some { status = Riot_executor.Action_executor.Executed; _ },
          Some { status = Riot_executor.Action_executor.Cached _; _ },
          Ok first_content,
          Ok second_content ->
            if exists1 && exists2 && String.equal first_content "cached output" && String.equal second_content "cached output" then
              Ok ()
            else
              Error "expected both sandboxes to materialize identical cached output"
        | _ -> Error "expected first run executed and second run cached")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_cache_misses_when_action_changes = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"integration_cache_changed"
      (fun tmpdir ->
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let graph1, node1 = make_graph_with_write ~package ~content:"v1" in
        let result1, exists1, _, _sandbox1 = execute_graph ~workspace ~store ~package ~graph:graph1 in
        let graph2, node2 = make_graph_with_write ~package ~content:"v2" in
        let result2, exists2, content2, _sandbox2 = execute_graph ~workspace ~store ~package ~graph:graph2 in
        match
          ( HashMap.get result1.Riot_executor.Action_executor.completed ~key:(node_id node1),
            HashMap.get result2.Riot_executor.Action_executor.completed ~key:(node_id node2),
            content2 )
        with
        | Some { status = Riot_executor.Action_executor.Executed; _ },
          Some { status = Riot_executor.Action_executor.Executed; _ },
          Ok second_content ->
            if exists1 && exists2 && String.equal second_content "v2" then
              Ok ()
            else
              Error "expected changed action to execute freshly with new output"
        | _ -> Error "expected changed action to miss cache")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dependency_change_invalidates_cached_compile_actions = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"integration_dep_cache_invalidates"
      (fun tmpdir ->
        let dep =
          make_library_package
            ~root:tmpdir
            ~name:"dep"
            ~interface:"val value : int\n"
            "let value = 1\n"
        in
        let app =
          make_library_package
            ~root:tmpdir
            ~name:"app"
            ~interface:"val value : int\n"
            ~dependencies:[ "dep" ]
            "let value = Dep.value\n"
        in
        let workspace = make_workspace tmpdir |> fun ws -> { ws with packages = [ dep; app ] } in
        let store = Riot_store.Store.create ~workspace in
        let first_graph =
          Riot_planner.Package_graph.create
            ~scope:Riot_planner.Package_graph.Runtime
            workspace
          |> Result.expect ~msg:"failed to create first package graph"
        in
        let first_dep = build_package ~workspace ~store ~package:dep ~package_graph:first_graph in
        match first_dep.status with
        | Riot_executor.Package_builder.Failed err ->
            Error ("first dependency build failed: " ^ Riot_executor.Package_builder.package_error_to_string err)
        | Riot_executor.Package_builder.Skipped { reason } ->
            Error ("first dependency build skipped: " ^ reason)
        | Riot_executor.Package_builder.Cached _
        | Riot_executor.Package_builder.Built _ -> (
            match execute_planned_package ~workspace ~store ~package:app ~package_graph:first_graph with
            | Error _ as err ->
                err
            | Ok _first_app_result ->
                let dep_source = Path.(dep.path / Path.v "src" / Path.v "dep.ml") in
                let _ = Fs.write "let value = 2\n" dep_source |> Result.expect ~msg:"failed to rewrite dependency source" in
                let second_graph =
                  Riot_planner.Package_graph.create
                    ~scope:Riot_planner.Package_graph.Runtime
                    workspace
                  |> Result.expect ~msg:"failed to create second package graph"
                in
                let second_dep = build_package ~workspace ~store ~package:dep ~package_graph:second_graph in
                match second_dep.status with
                | Riot_executor.Package_builder.Failed err ->
                    Error ("second dependency build failed: " ^ Riot_executor.Package_builder.package_error_to_string err)
                | Riot_executor.Package_builder.Skipped { reason } ->
                    Error ("second dependency build skipped: " ^ reason)
                | Riot_executor.Package_builder.Cached _ ->
                    Error "expected dependency source edit to miss package cache"
                | Riot_executor.Package_builder.Built _ -> (
                    match execute_planned_package ~workspace ~store ~package:app ~package_graph:second_graph with
                    | Error _ as err ->
                        err
                    | Ok second_app_result ->
                        let statuses =
                          HashMap.to_list second_app_result.Riot_executor.Action_executor.completed
                          |> List.map ~fn:(fun (_, result) -> result.Riot_executor.Action_executor.status)
                        in
                        let cached_count =
                          List.fold_left statuses ~acc:0 ~fn:(fun count status ->
                            match status with
                            | Riot_executor.Action_executor.Cached _ -> count + 1
                            | Riot_executor.Action_executor.Executed
                            | Riot_executor.Action_executor.Failed _
                            | Riot_executor.Action_executor.Skipped -> count)
                        in
                        if Int.equal cached_count 0 then
                          Ok ()
                        else
                          Error ("expected dependency change to invalidate all cached app actions, got "
                          ^ Int.to_string cached_count
                          ^ " cached actions")
                  )
          )
      )
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case "equivalent action graph reuses cached artifact" test_execute_reuses_cache_for_equivalent_graph;
    case "changed action graph misses cache" test_execute_cache_misses_when_action_changes;
    case "dependency change invalidates cached compile actions" test_dependency_change_invalidates_cached_compile_actions;
  ]

let name = "riot-executor:integration-caching"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
