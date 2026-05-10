open Std
open Riot_build
open Std.Collections
open Riot_model

module Action_executor = Riot_build.Internal.Action_executor
module Test = Std.Test

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let test_toolchain = fun () ->
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let test_build_target = Riot_model.Target.current

let make_workspace = fun root ->
  Riot_model.Workspace.make
    ~root
    ~target_dir:(Path.v "target")
    ~packages:[]
    ()

let read_file = fun path ->
  Fs.read_to_string path
  |> Result.expect ~msg:"failed to read file"

let make_package = fun ~root ~name ->
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Riot_model.Package.make
    ~name:(package_name name)
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_node = fun ~package ~srcs ->
  let graph = Riot_planner.Action_graph.create () in
  let spec =
    Riot_planner.Action_node.make
      ~actions:[
        Riot_planner.Action.WriteFile { destination = Path.v "out.txt"; content = "ok" };
      ]
      ~outs:[ Path.v "out.txt" ]
      ~srcs
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  Riot_planner.Action_graph.add_node graph spec

let make_cache_node = fun ~package ~content ->
  let graph = Riot_planner.Action_graph.create () in
  let spec =
    Riot_planner.Action_node.make
      ~actions:[ Riot_planner.Action.WriteFile { destination = Path.v "out.txt"; content } ]
      ~outs:[ Path.v "out.txt" ]
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  Riot_planner.Action_graph.add_node graph spec

let with_workspace_dirs = fun tmpdir f ->
  let package_src = Path.(tmpdir / Path.v "packages" / Path.v "kernel" / Path.v "src") in
  match Fs.create_dir_all package_src with
  | Error _ -> Error "create package src failed"
  | Ok () ->
      match Fs.write "let kernel = 1" Path.(package_src / Path.v "lib.ml") with
      | Error _ -> Error "write package source failed"
      | Ok () ->
          let sandbox = Path.(tmpdir / Path.v "sandbox") in
          match Fs.create_dir_all sandbox with
          | Error _ -> Error "create sandbox failed"
          | Ok () -> f sandbox

let test_execute_node_copies_package_relative_sources = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_exec_src_copy_rel"
    (fun tmpdir ->
      with_workspace_dirs
        tmpdir
        (fun sandbox ->
          let workspace = make_workspace tmpdir in
          let store = Riot_store.Store.create ~workspace in
          let package = make_package ~root:tmpdir ~name:"kernel" in
          let node = make_node ~package ~srcs:[ Path.v "src/lib.ml" ] in
          let completed = ConcurrentHashMap.create () in
          let result =
            Action_executor.execute_node
              ~completed
              ~store
              ~session_id:(Riot_model.Session_id.make ())
              ~build_target:test_build_target
              (test_toolchain ())
              sandbox
              node
          in
          match result.status with
          | Action_executor.Executed _ ->
              let copied = Path.(sandbox / Path.v "src/lib.ml") in
              (match Fs.exists copied with
              | Ok true -> Ok ()
              | _ -> Error "expected package-relative source to be copied")
          | _ -> Error "expected node execution to succeed")) with
  | Ok r -> r
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_copies_workspace_relative_sources = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_exec_src_copy_ws"
    (fun tmpdir ->
      with_workspace_dirs
        tmpdir
        (fun sandbox ->
          let workspace = make_workspace tmpdir in
          let store = Riot_store.Store.create ~workspace in
          let package = make_package ~root:tmpdir ~name:"kernel" in
          let node = make_node ~package ~srcs:[ Path.v "packages/kernel/src/lib.ml" ] in
          let completed = ConcurrentHashMap.create () in
          let result =
            Action_executor.execute_node
              ~completed
              ~store
              ~session_id:(Riot_model.Session_id.make ())
              ~build_target:test_build_target
              (test_toolchain ())
              sandbox
              node
          in
          match result.status with
          | Action_executor.Executed _ ->
              let copied = Path.(sandbox / Path.v "packages/kernel/src/lib.ml") in
              (match Fs.exists copied with
              | Ok true -> Ok ()
              | _ -> Error "expected workspace-relative source to be copied")
          | _ -> Error "expected node execution to succeed")) with
  | Ok r -> r
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_cache_hit_materializes_outputs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_exec_cache_hit"
    (fun tmpdir ->
      let sandbox = Path.(tmpdir / Path.v "sandbox") in
      let _ =
        Fs.create_dir_all sandbox
        |> Result.expect ~msg:"create sandbox failed"
      in
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"kernel" in
      let node = make_cache_node ~package ~content:"cached output" in
      let completed = ConcurrentHashMap.create () in
      let session_id = Riot_model.Session_id.make () in
      let toolchain = test_toolchain () in
      let first =
        Action_executor.execute_node
          ~completed
          ~store
          ~session_id
          ~build_target:test_build_target
          toolchain
          sandbox
          node
      in
      match first.status with
      | Action_executor.Executed _ ->
          let output = Path.(sandbox / Path.v "out.txt") in
          let _ =
            Fs.remove_file output
            |> Result.expect ~msg:"remove cached output failed"
          in
          let second =
            Action_executor.execute_node
              ~completed
              ~store
              ~session_id
              ~build_target:test_build_target
              toolchain
              sandbox
              node
          in
          (
            match second.status with
            | Action_executor.Cached _ ->
                (match Fs.exists output with
                | Ok true ->
                    if String.equal (read_file output) "cached output" then
                      Ok ()
                    else
                      Error "cached output content mismatch"
                | _ -> Error "expected cached output to be materialized")
            | _ -> Error "expected second execution to hit cache"
          )
      | _ -> Error "expected first execution to populate cache") with
  | Ok r -> r
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case
      "execute_node copies package-relative sources"
      test_execute_node_copies_package_relative_sources;
    case
      "execute_node copies workspace-relative sources"
      test_execute_node_copies_workspace_relative_sources;
    case
      "execute_node cache hit materializes outputs"
      test_execute_node_cache_hit_materializes_outputs;
  ]

let name = "riot-build:action-executor-source-copy"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
