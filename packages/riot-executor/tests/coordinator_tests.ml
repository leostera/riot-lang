open Std
module Test = Std.Test

let make_build_ctx = fun ?(parallelism = 2) () ->
  let session_id = Riot_model.Session_id.make () in
  Riot_model.Build_ctx.make
    ~session_id
    ~profile:Riot_model.Profile.debug
    ~available_parallelism:parallelism
    ()

let write_package = fun ~root ~name ~lib_body ~deps ->
  let pkg_dir = Path.(root / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src failed" in
  let _ = Fs.write lib_body Path.(src_dir / Path.v "lib.ml") |> Result.expect ~msg:"write source failed" in
  let deps_block =
    match deps with
    | [] -> ""
    | _ -> "\n[dependencies]\n"
    ^ String.concat "\n" (List.map (fun dep -> dep ^ " = \"*\"") deps)
    ^ "\n"
  in
  let riot_toml = "[package]\nname = \""
  ^ name
  ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
  ^ deps_block in
  let _ = Fs.write riot_toml Path.(pkg_dir / Path.v "riot.toml") |> Result.expect ~msg:"write riot.toml failed" in
  ()

let write_workspace = fun ~root members ->
  let riot_toml = "[workspace]\nmembers = ["
  ^ String.concat ", " (List.map (fun member -> "\"" ^ member ^ "\"") members)
  ^ "]\n" in
  let _ = Fs.write riot_toml Path.(root / Path.v "riot.toml") |> Result.expect ~msg:"write workspace riot.toml failed" in
  ()

let with_scanned_workspace = fun tmpdir f ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan workspace_manager tmpdir with
  | Error _ -> Error "workspace scan failed"
  | Ok (workspace, _load_errors) -> f workspace

let result_status_to_string = fun (result: Riot_executor.Package_builder.build_result) ->
  let status =
    match result.status with
    | Riot_executor.Package_builder.Cached _ -> "cached"
    | Built _ -> "built"
    | Skipped { reason } -> "skipped(" ^ reason ^ ")"
    | Failed err -> "failed(" ^ Riot_executor.Package_builder.package_error_to_string err ^ ")"
  in
  result.package.Riot_model.Package.name ^ ":" ^ status

let test_build_workspace_two_packages_success = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"coordinator_two_pkg_test"
      (fun tmpdir ->
        write_package ~root:tmpdir ~name:"a" ~lib_body:"let a = 1" ~deps:[];
        write_package ~root:tmpdir ~name:"b" ~lib_body:"let b = 1" ~deps:[ "a" ];
        write_workspace ~root:tmpdir [ "a"; "b" ];
        with_scanned_workspace tmpdir
          (fun workspace ->
            let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
            |> Result.expect ~msg:"toolchain init failed" in
            let store = Riot_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:2 () in
            match Riot_executor.Coordinator.build_workspace
              ~workspace
              ~toolchain
              ~store
              ~target:Riot_planner.Workspace_planner.All
              ~scope:Riot_planner.Package_graph.Runtime
              ~concurrency:2
              ~build_ctx
              ~session_id:build_ctx.Riot_model.Build_ctx.session_id with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                if List.length result.results = 2 && result.failed_count = 0 then
                  Ok ()
                else
                  Error ("unexpected workspace result accounting: results="
                  ^ Int.to_string (List.length result.results)
                  ^ " failed_count="
                  ^ Int.to_string result.failed_count
                  ^ " statuses=["
                  ^ String.concat ", " (List.map result_status_to_string result.results)
                  ^ "]")))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_build_workspace_respects_serial_package_orchestration = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"coordinator_serial_pkg_test"
      (fun tmpdir ->
        write_package ~root:tmpdir ~name:"left" ~lib_body:"let x = 1" ~deps:[];
        write_package ~root:tmpdir ~name:"right" ~lib_body:"let y = 2" ~deps:[];
        write_workspace ~root:tmpdir [ "left"; "right" ];
        with_scanned_workspace tmpdir
          (fun workspace ->
            let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
            |> Result.expect ~msg:"toolchain init failed" in
            let store = Riot_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:1 () in
            match Riot_executor.Coordinator.build_workspace
              ~workspace
              ~toolchain
              ~store
              ~target:Riot_planner.Workspace_planner.All
              ~scope:Riot_planner.Package_graph.Runtime
              ~concurrency:4
              ~build_ctx
              ~session_id:build_ctx.Riot_model.Build_ctx.session_id with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                if List.length result.results = 2 && result.failed_count = 0 then
                  Ok ()
                else
                  Error ("serial orchestration build should succeed: results="
                  ^ Int.to_string (List.length result.results)
                  ^ " failed_count="
                  ^ Int.to_string result.failed_count
                  ^ " statuses=["
                  ^ String.concat ", " (List.map result_status_to_string result.results)
                  ^ "]")))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_failed_dependency_updates_package_graph = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"coordinator_failed_dep_test"
      (fun tmpdir ->
        write_package ~root:tmpdir ~name:"a" ~lib_body:"let broken =" ~deps:[];
        write_package ~root:tmpdir ~name:"b" ~lib_body:"let b = 1" ~deps:[ "a" ];
        write_workspace ~root:tmpdir [ "a"; "b" ];
        with_scanned_workspace tmpdir
          (fun workspace ->
            let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
            |> Result.expect ~msg:"toolchain init failed" in
            let store = Riot_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:2 () in
            match Riot_executor.Coordinator.build_workspace
              ~workspace
              ~toolchain
              ~store
              ~target:Riot_planner.Workspace_planner.All
              ~scope:Riot_planner.Package_graph.Runtime
              ~concurrency:2
              ~build_ctx
              ~session_id:build_ctx.Riot_model.Build_ctx.session_id with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                let package_key = Riot_planner.Package_graph.package_key
                  ~package_name:"b"
                  Riot_planner.Package_graph.Runtime in
                match Riot_planner.Package_graph.get_node_by_key result.package_graph package_key with
                | None ->
                    let graph_nodes = Riot_planner.Package_graph.topological_sort result.package_graph
                    |> List.map
                      (fun node -> Riot_planner.Package_graph.get_key node |> Riot_model.Package.key_to_string) in
                    Error ("missing package graph node for failed package; graph keys=["
                    ^ String.concat ", " graph_nodes
                    ^ "]")
                | Some node -> (
                    match node.value with
                    | Riot_planner.Package_graph.Failed _ -> Ok ()
                    | Riot_planner.Package_graph.Unplanned _ ->
                        Error "package graph left dependent package unplanned after \
                           dependency failure"
                    | Riot_planner.Package_graph.Planned _ ->
                        Error "package graph left dependent package planned after \
                           dependency failure"
                    | Riot_planner.Package_graph.Cached _ ->
                        Error "package graph left dependent package cached after \
                           dependency failure"
                    | Riot_planner.Package_graph.Built _ ->
                        Error "package graph left dependent package built after \
                           dependency failure"
                    | Riot_planner.Package_graph.Skipped _ -> Ok ()
                  )))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.[
    case ~size:Large "build_workspace: two packages succeed" test_build_workspace_two_packages_success;
    case "build_workspace: serial orchestration succeeds" test_build_workspace_respects_serial_package_orchestration;
    case "build_workspace: dependency failure updates package graph" test_failed_dependency_updates_package_graph;
  ]

let name = "Coordinator Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
