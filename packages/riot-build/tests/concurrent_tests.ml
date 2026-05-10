open Std
open Riot_build

module Action_scheduler = Riot_build.Internal.Action_scheduler
module Package_builder = Riot_build.Internal.Package_builder
module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let make_test_build_ctx = fun () ->
  let session_id = Riot_model.Session_id.make () in
  Riot_model.Build_ctx.make ~session_id ~profile:Riot_model.Profile.debug ()

let unit_key = fun package ->
  ({
    package = package.Riot_model.Package.name;
    artifact = Riot_planner.Build_unit.Library;
    target = Riot_model.Target.host ();
    profile = Riot_model.Profile.debug;
  }: Riot_planner.Build_unit.key)

let build_package = fun ~workspace ~toolchain ~store package ->
  let build_ctx = make_test_build_ctx () in
  let input_hash_cache = Riot_planner.Package_planner.create_input_hash_cache () in
  let key = unit_key package in
  let unit =
    Riot_planner.Build_unit.from_artifact
      ~package
      ~artifact:key.artifact
      ~target:key.target
      ~profile:key.profile
  in
  let detailed_result =
    match Package_builder.plan_build_unit
      ~on_source_analyzed:(fun _ -> ())
      ~input_hash_cache
      ~workspace
      ~toolchain
      ~store
      ~unit
      ~depset:[]
      ~build_ctx
      ~emit_visible_progress:false with
    | Package_builder.Final_result detailed_result -> detailed_result
    | Execution_required execution_plan ->
        match Package_builder.prepare_execution
          ~workspace
          ~toolchain
          ~store
          ~execution_plan
          ~build_ctx with
        | Error detailed_result -> detailed_result
        | Ok prepared_execution ->
            let action_result =
              Action_scheduler.run
                ~action_graph:execution_plan.action_graph
                ~sandbox:prepared_execution.sandbox
                ~store
                ~session_id:build_ctx.session_id
                ~build_target:(Riot_model.Target.host ())
                prepared_execution.toolchain
                ~concurrency:build_ctx.parallelism
            in
            let completed = Std.Collections.ConcurrentHashMap.create () in
            List.for_each
              action_result.completed_actions
              ~fn:(fun completed_action ->
                let _ =
                  Std.Collections.ConcurrentHashMap.insert
                    completed
                    ~key:(Riot_planner.Action_node.id completed_action.node)
                    ~value:completed_action.result
                in
                ());
            Package_builder.finalize_execution
              ~workspace
              ~store
              ~prepared_execution
              ~completed
              ~build_ctx
  in
  match detailed_result.Package_builder.result.status with
  | Built _
  | Cached _ -> Ok detailed_result.result
  | Skipped { reason } -> Error ("skipped: " ^ reason)
  | Failed err -> Error (Package_builder.package_error_to_string err)

let make_package = fun tmpdir name content ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let package_name = package_name name in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"Create src failed"
  in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ =
    Fs.write content ml_file
    |> Result.expect ~msg:"Write ml failed"
  in
  let riot_file = Path.(pkg_dir / Path.v "riot.toml") in
  let riot_content =
    "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
  in
  let _ =
    Fs.write riot_content riot_file
    |> Result.expect ~msg:"Write riot.toml"
  in
  Riot_model.Package.make
    ~name:package_name
    ~path:pkg_dir
    ~relative_path:(Path.v name)
    ~library:{ path = Path.v "src/lib.ml" }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

type Message.t +=
  | BuildComplete of (string * (unit, string) result)

type Message.t +=
  | BuildCompleteWithCache of (string * bool * (unit, string) result)

let test_concurrent_builds_different_packages = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"concurrent_test"
    (fun tmpdir ->
      let pkg1 = make_package tmpdir "pkg-1" "let x = 1" in
      let pkg2 = make_package tmpdir "pkg-2" "let x = 2" in
      let workspace =
        Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ pkg1; pkg2 ]
          ~target_dir:(Path.v "target")
          ()
      in
      let toolchain =
        Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain"
      in
      let store = Riot_store.Store.create ~workspace in
      let parent = self () in
      let _worker1 =
        spawn
          (fun () ->
            let status =
              Result.map (build_package ~workspace ~toolchain ~store pkg1) ~fn:(fun _ -> ())
            in
            send parent (BuildComplete ("pkg-1", status));
            Ok ())
      in
      let _worker2 =
        spawn
          (fun () ->
            let status =
              Result.map (build_package ~workspace ~toolchain ~store pkg2) ~fn:(fun _ -> ())
            in
            send parent (BuildComplete ("pkg-2", status));
            Ok ())
      in
      let selector msg =
        match msg with
        | BuildComplete _ -> Select msg
        | _ -> Skip
      in
      let result1 = receive ~selector () in
      let result2 = receive ~selector () in
      match (result1, result2) with
      | (BuildComplete (name1, Ok ()), BuildComplete (name2, Ok ())) ->
          if (name1 = "pkg-1" && name2 = "pkg-2") || (name1 = "pkg-2" && name2 = "pkg-1") then
            Ok ()
          else
            Error ("Unexpected package names: " ^ name1 ^ ", " ^ name2)
      | (BuildComplete (name, Error err), _) -> Error (name ^ " build failed: " ^ err)
      | (_, BuildComplete (name, Error err)) -> Error (name ^ " build failed: " ^ err)
      | _ -> Error "Unexpected message") with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_concurrent_builds_same_package = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"concurrent_test"
    (fun tmpdir ->
      let package = make_package tmpdir "test-pkg" "let x = 42" in
      let workspace =
        Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:(Path.v "target")
          ()
      in
      let toolchain =
        Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain"
      in
      let store = Riot_store.Store.create ~workspace in
      let parent = self () in
      let _worker1 =
        spawn
          (fun () ->
            let status =
              Result.map (build_package ~workspace ~toolchain ~store package) ~fn:(fun _ -> ())
            in
            send parent (BuildComplete ("worker1", status));
            Ok ())
      in
      let _worker2 =
        spawn
          (fun () ->
            let status =
              Result.map (build_package ~workspace ~toolchain ~store package) ~fn:(fun _ -> ())
            in
            send parent (BuildComplete ("worker2", status));
            Ok ())
      in
      let selector msg =
        match msg with
        | BuildComplete _ -> Select msg
        | _ -> Skip
      in
      let result1 = receive ~selector () in
      let result2 = receive ~selector () in
      match (result1, result2) with
      | (BuildComplete (_, Ok ()), BuildComplete (_, Ok ())) -> Ok ()
      | (BuildComplete (name, Error err), _) -> Error (name ^ " build failed: " ^ err)
      | (_, BuildComplete (name, Error err)) -> Error (name ^ " build failed: " ^ err)
      | _ -> Error "Unexpected message") with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_concurrent_builds_with_shared_cache = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"concurrent_test"
    (fun tmpdir ->
      let package = make_package tmpdir "test-pkg" "let x = 42" in
      let workspace =
        Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:(Path.v "target")
          ()
      in
      let toolchain =
        Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain"
      in
      let store = Riot_store.Store.create ~workspace in
      let first_build = build_package ~workspace ~toolchain ~store package in
      match first_build with
      | Ok first_result -> (
          match first_result.Package_builder.status with
          | Cached _ -> Error "first build should not be cached"
          | Skipped { reason } -> Error ("first build was unexpectedly skipped: " ^ reason)
          | Failed err ->
              Error ("first build failed: " ^ Package_builder.package_error_to_string err)
          | Built _ ->
              let parent = self () in
              let _worker1 =
                spawn
                  (fun () ->
                    let result = build_package ~workspace ~toolchain ~store package in
                    let cached =
                      match Result.map result ~fn:(fun result -> result.Package_builder.status) with
                      | Ok (Cached _) -> true
                      | Ok (Built _)
                      | Ok (Skipped _)
                      | Ok (Failed _)
                      | Error _ -> false
                    in
                    let status = Result.map result ~fn:(fun _ -> ()) in
                    send parent (BuildCompleteWithCache ("worker1", cached, status));
                    Ok ())
              in
              let _worker2 =
                spawn
                  (fun () ->
                    let result = build_package ~workspace ~toolchain ~store package in
                    let cached =
                      match Result.map result ~fn:(fun result -> result.Package_builder.status) with
                      | Ok (Cached _) -> true
                      | Ok (Built _)
                      | Ok (Skipped _)
                      | Ok (Failed _)
                      | Error _ -> false
                    in
                    let status = Result.map result ~fn:(fun _ -> ()) in
                    send parent (BuildCompleteWithCache ("worker2", cached, status));
                    Ok ())
              in
              let selector msg =
                match msg with
                | BuildCompleteWithCache _ -> Select msg
                | _ -> Skip
              in
              let result1 = receive ~selector () in
              let result2 = receive ~selector () in
              match (result1, result2) with
              | (
                  BuildCompleteWithCache (_, cached1, Ok ()),
                  BuildCompleteWithCache (_, cached2, Ok ())
                ) ->
                  let _ = cached1 in
                  let _ = cached2 in
                  Ok ()
              | (BuildCompleteWithCache (name, _, Error err), _) ->
                  Error (name ^ " build failed: " ^ err)
              | (_, BuildCompleteWithCache (name, _, Error err)) ->
                  Error (name ^ " build failed: " ^ err)
              | _ -> Error "Unexpected message"
        )
      | Error err -> Error ("First build failed: " ^ err)) with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests = let open Test in
[
  case
    ~size:Large
    "concurrent: different packages don't interfere"
    test_concurrent_builds_different_packages;
  case ~size:Large "concurrent: same package builds safely" test_concurrent_builds_same_package;
  case
    ~size:Large
    "concurrent: shared cache works correctly"
    test_concurrent_builds_with_shared_cache;
]

let name = "Concurrent Build Tests"

let main ~args = Test.Cli.main ~execution_mode:Test.Cli.Linear ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
