open Std
module Test = Std.Test
module G = Graph.SimpleGraph

let test_toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
|> Result.expect ~msg:"Failed to initialize toolchain"

let planner_artifacts_version = "planner-artifacts:v11"

let make_test_workspace = fun tmpdir packages ->
  Riot_model.Workspace.{
    name = None;
    root = tmpdir;
    target_dir_root =
      Path.(tmpdir / Path.v "target");
    packages;
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    profile_overrides = [];
  }

let make_package = fun tmpdir name ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let _ = Fs.create_dir_all pkg_dir in
  Riot_model.Package.make ~name ~path:pkg_dir ~relative_path:(Path.v name) ()

let compute_input_hash = fun ?(planner_version = planner_artifacts_version) ~package ~workspace ~profile ~build_ctx () ->
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in
  H.write state planner_version;
  Riot_model.Build_ctx.hash state build_ctx;
  H.write_hash state (Riot_toolchain.hash test_toolchain);
  Riot_model.Package.hash state package;
  let sorted_deps =
    List.sort
      (Riot_model.Package.build_graph_dependencies package)
      ~compare:(fun (a: Riot_model.Package.dependency) (b: Riot_model.Package.dependency) ->
        String.compare a.name b.name)
  in
  List.for_each
    sorted_deps
    ~fn:(fun (dep: Riot_model.Package.dependency) ->
      match dep.source with
      | { workspace=true; _ } -> (
          match List.find
            workspace.Riot_model.Workspace.packages
            ~fn:(fun (p: Riot_model.Package.t) -> p.name = dep.name)
          with
          | Some dep_pkg ->
              H.write state (Path.to_string dep_pkg.path);
              H.write state
                (
                  if Option.is_some dep_pkg.library then
                    "true"
                  else
                    "false"
                )
          | None -> ()
        )
      | { builtin=true; _ } ->
          ()
      | _ ->
          ())
  ;
  H.finish state

let test_plan_bundle_cache_hit_restores_module_and_action_graphs = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_bundle_hit_test"
      (fun tmpdir ->
        let package = make_package tmpdir "pkg" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let input_hash = compute_input_hash ~package ~workspace ~profile ~build_ctx () in
        let action_graph_json =
          let ag = Riot_planner.Action_graph.create () in
          let action = Riot_planner.Action.WriteFile {
            destination = Path.v "out.txt";
            content = "cached"
          } in
          let spec =
            Riot_planner.Action_node.make
              ~actions:[ action ]
              ~outs:[ Path.v "out.txt" ]
              ~srcs:[]
              ~package
              ~toolchain:test_toolchain
              ~dependency_hashes:(fun _ -> Crypto.hash_string "")
              ~deps:[]
          in
          let _ = Riot_planner.Action_graph.add_node ag spec in
          Riot_planner.Action_graph.to_json ag
        in
        let module_graph_json = Std.Data.Json.Object [
          (
            "nodes",
            Std.Data.Json.Array [
              Std.Data.Json.Object [
                ("id", Std.Data.Json.Int 1);
                (
                  "file",
                  Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "");
                  ]
                );
                ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root") ]);
                ("deps", Std.Data.Json.Array []);
                ("opens", Std.Data.Json.Array []);
              ];
            ]
          );
        ] in
        let bundle = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String package.name);
          ("module_graph", module_graph_json);
          ("action_graph", action_graph_json);
        ] in
        let _ = Riot_store.Store.save_plan_bundle store ~hash:input_hash ~plan:bundle
        |> Result.expect ~msg:"save_plan_bundle should succeed" in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.expect ~msg:"package graph should build" in
        let package_key = Riot_planner.Package_graph.package_key
          ~package_name:package.name
          Riot_planner.Package_graph.Runtime in
        match Riot_planner.Package_planner.plan_package
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key
          ~package
          ~build_ctx with
        | Error err ->
            Error ("expected cache-hit plan result, got planner error: "
            ^ Riot_planner.Planning_error.to_string err)
        | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; _ }) ->
            let module_nodes =
              match G.topo_sort module_graph with
              | Ok nodes -> nodes
              | Error _ -> []
            in
            let action_nodes = Riot_planner.Action_graph.nodes action_graph in
            if List.length module_nodes = 1 && List.length action_nodes = 1 then
              Ok ()
            else
              Error ("expected restored module/action graphs with one node each, got "
              ^ Int.to_string (List.length module_nodes)
              ^ " module nodes and "
              ^ Int.to_string (List.length action_nodes)
              ^ " action nodes")
        | Ok _ ->
            Error "expected Planned result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_cached_artifact_and_exports_short_circuit_without_plan_bundle = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_cached_artifact_hit_test"
      (fun tmpdir ->
        let package = make_package tmpdir "pkg" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let input_hash = compute_input_hash ~package ~workspace ~profile ~build_ctx () in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let output = Path.(sandbox_dir / Path.v "pkg.cma") in
        let _ = Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"sandbox dir creation should succeed" in
        let _ = Fs.write "cached" output |> Result.expect ~msg:"artifact output write should succeed" in
        let exports = [
          Riot_store.Store.{
            name = "pkg.cma";
            path = Path.v "pkg.cma";
            action_hash = Std.Crypto.Digest.hex input_hash
          };
        ] in
        let _artifact = Riot_store.Store.save
          store
          ~package:package.name
          ~exports
          ~hash:input_hash
          ~sandbox_dir
          ~outs:[ output ]
        |> Result.expect ~msg:"artifact save should succeed" in
        if Option.is_some (Riot_store.Store.load_plan_bundle store ~hash:input_hash) then
          Error "expected no plan bundle before cached planner lookup"
        else
          let package_graph = Riot_planner.Package_graph.create
            ~scope:Riot_planner.Package_graph.Runtime workspace
          |> Result.expect ~msg:"package graph should build" in
          let package_key = Riot_planner.Package_graph.package_key
            ~package_name:package.name
            Riot_planner.Package_graph.Runtime in
          match Riot_planner.Package_planner.plan_package
            ~workspace
            ~toolchain:test_toolchain
            ~store
            ~package_graph
            ~package_key
            ~package
            ~build_ctx with
          | Error err -> Error ("expected cached plan result, got planner error: "
          ^ Riot_planner.Planning_error.to_string err)
          | Ok (Riot_planner.Package_planner.Cached {
            hash;
            artifact=cached_artifact;
            exports=cached_exports;
            _
          }) ->
              if not (Std.Crypto.Hash.compare hash input_hash = 0) then
                Error "expected cached plan hash to match input hash"
              else if not (List.length cached_artifact.Riot_store.Artifact.files = 1) then
                Error "expected cached artifact to expose one file"
              else if not (List.length cached_exports = 1) then
                Error "expected cached export manifest to expose one export"
              else
                Ok ()
          | Ok _ -> Error "expected Cached result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_stale_plan_bundle_version_rebuilds_plan_graphs = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_bundle_stale_version_test"
      (fun tmpdir ->
        let package = Riot_model.Package.make ~name:"pkg" ~path:Path.(tmpdir / Path.v "pkg") ~relative_path:(Path.v
          "pkg") ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [ Path.v "src/pkg.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let src_dir = Path.(package.path / Path.v "src") in
        let source = Path.(src_dir / Path.v "pkg.ml") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "let value = 1\n" source |> Result.expect ~msg:"expected source write to succeed" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.release in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let stale_input_hash = compute_input_hash
          ~planner_version:"planner-artifacts:v2"
          ~package
          ~workspace
          ~profile
          ~build_ctx
          () in
        let stale_action_graph_json =
          let ag = Riot_planner.Action_graph.create () in
          let action = Riot_planner.Action.WriteFile {
            destination = Path.v "out.txt";
            content = "stale"
          } in
          let spec =
            Riot_planner.Action_node.make
              ~actions:[ action ]
              ~outs:[ Path.v "out.txt" ]
              ~srcs:[]
              ~package
              ~toolchain:test_toolchain
              ~dependency_hashes:(fun _ -> Crypto.hash_string "")
              ~deps:[]
          in
          let _ = Riot_planner.Action_graph.add_node ag spec in
          Riot_planner.Action_graph.to_json ag
        in
        let stale_module_graph_json = Std.Data.Json.Object [
          (
            "nodes",
            Std.Data.Json.Array [
              Std.Data.Json.Object [
                ("id", Std.Data.Json.Int 1);
                (
                  "file",
                  Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "");
                  ]
                );
                ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root") ]);
                ("deps", Std.Data.Json.Array []);
                ("opens", Std.Data.Json.Array []);
              ];
            ]
          );
        ] in
        let stale_bundle = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String package.name);
          ("module_graph", stale_module_graph_json);
          ("action_graph", stale_action_graph_json);
        ] in
        let _ = Riot_store.Store.save_plan_bundle store ~hash:stale_input_hash ~plan:stale_bundle
        |> Result.expect ~msg:"expected stale plan bundle save to succeed" in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.expect ~msg:"package graph should build" in
        let package_key = Riot_planner.Package_graph.package_key
          ~package_name:package.name
          Riot_planner.Package_graph.Runtime in
        match Riot_planner.Package_planner.plan_package
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key
          ~package
          ~build_ctx with
        | Error err ->
            Error ("expected stale bundle miss to replan package, got planner error: "
            ^ Riot_planner.Planning_error.to_string err)
        | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) ->
            let actions = Riot_planner.Action_graph.to_action_list action_graph in
            if List.any
                actions
                ~fn:(
                  function
                  | Riot_planner.Action.CreateLibrary _ -> true
                  | _ -> false
                )
            then
              Ok ()
            else
              Error "expected stale plan bundle to be ignored and rebuilt"
        | Ok _ ->
            Error "expected Planned result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.[
    case "plan bundle cache hit restores module and action graphs" test_plan_bundle_cache_hit_restores_module_and_action_graphs;
    case "cached artifact and exports short-circuit without plan bundle" test_cached_artifact_and_exports_short_circuit_without_plan_bundle;
    case "stale plan bundle version rebuilds plan graphs" test_stale_plan_bundle_version_rebuilds_plan_graphs;
  ]

let name = "Planner Package Planning Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
