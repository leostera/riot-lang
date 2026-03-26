open Std

module Test = Std.Test
module G = Std.Graph.SimpleGraph

let test_toolchain =
  Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize toolchain"

let make_package name =
  Tusk_model.Package.
    {
      name;
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let make_package_with_paths ~name ~path ~relative_path =
  Tusk_model.Package.
    {
      name;
      path;
      relative_path;
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let test_action_graph_json_round_trip_preserves_dependencies () =
  let package = make_package "pkg" in
  let graph = Tusk_planner.Action_graph.create () in

  let write_a =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "a.txt"; content = "a" }
  in
  let spec_a =
    Tusk_planner.Action_node.make ~actions:[ write_a ] ~outs:[ Path.v "a.txt" ]
      ~srcs:[] ~package ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "") ~deps:[]
  in
  let node_a = Tusk_planner.Action_graph.add_node graph spec_a in

  let write_b =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "b.txt"; content = "b" }
  in
  let spec_b =
    Tusk_planner.Action_node.make ~actions:[ write_b ] ~outs:[ Path.v "b.txt" ]
      ~srcs:[] ~package ~toolchain:test_toolchain
      ~dependency_hashes:(fun dep_id ->
        if Graph.SimpleGraph.Node_id.eq dep_id node_a.id then
          Tusk_planner.Action_node.get_hash node_a
        else Crypto.hash_string "missing")
      ~deps:[ node_a.id ]
  in
  let node_b = Tusk_planner.Action_graph.add_node graph spec_b in
  Tusk_planner.Action_graph.add_dependency graph node_b ~depends_on:node_a;

  let encoded = Tusk_planner.Action_graph.to_json graph in
  match Tusk_planner.Action_graph.from_json encoded with
  | Error err -> Error ("round-trip decode failed: " ^ err)
  | Ok decoded -> (
      let encoded_decoded = Tusk_planner.Action_graph.to_json decoded in
      match Data.Json.get_field "nodes" encoded_decoded with
      | Some (Data.Json.Array node_jsons) ->
          let edge_count =
            List.fold_left
              (fun acc node_json ->
                match Data.Json.get_field "dependencies" node_json with
                | Some (Data.Json.Array deps) -> acc + List.length deps
                | _ -> acc)
              0 node_jsons
          in
          if List.length node_jsons = 2 && edge_count = 1 then Ok ()
          else
            Error
              ("expected 2 nodes and 1 edge, got "
             ^ Int.to_string (List.length node_jsons)
             ^ " nodes and " ^ Int.to_string edge_count ^ " edges")
      | _ -> Error "decoded graph missing nodes array")

let test_action_graph_json_round_trip_preserves_package_paths_and_hashes () =
  let package =
    make_package_with_paths ~name:"kernel" ~path:(Path.v "packages/kernel")
      ~relative_path:(Path.v "packages/kernel")
  in
  let graph = Tusk_planner.Action_graph.create () in
  let action =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "build/meta.txt"; content = "ok" }
  in
  let spec =
    Tusk_planner.Action_node.make ~actions:[ action ]
      ~outs:[ Path.v "build/meta.txt" ]
      ~srcs:[ Path.v "packages/kernel/src/lib.ml" ] ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "") ~deps:[]
  in
  let node = Tusk_planner.Action_graph.add_node graph spec in
  let expected_hash = Tusk_planner.Action_node.get_hash node in
  let encoded = Tusk_planner.Action_graph.to_json graph in
  match Tusk_planner.Action_graph.from_json encoded with
  | Error err -> Error ("round-trip decode failed: " ^ err)
  | Ok decoded -> (
      match Tusk_planner.Action_graph.nodes decoded with
      | [ decoded_node ] ->
          let decoded_path = decoded_node.value.package.Tusk_model.Package.path in
          let decoded_rel =
            decoded_node.value.package.Tusk_model.Package.relative_path
          in
          let decoded_hash = Tusk_planner.Action_node.get_hash decoded_node in
          if
            Path.equal decoded_path (Path.v "packages/kernel")
            && Path.equal decoded_rel (Path.v "packages/kernel")
            && Crypto.Hash.equal decoded_hash expected_hash
          then Ok ()
          else Error "package path/relative_path/hash did not round-trip"
      | _ -> Error "expected one decoded node")

let test_action_hash_tracks_package_relative_source_contents () =
  match
    Fs.with_tempdir ~prefix:"action_hash_pkg_src" (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
        let src_dir = Path.(package_root / Path.v "src") in
        let source = Path.(src_dir / Path.v "demo.ml") in
        let _ =
          Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed"
        in
        let package =
          make_package_with_paths ~name:"demo" ~path:package_root
            ~relative_path:(Path.v "packages/demo")
        in
        let action =
          Tusk_planner.Action.CompileImplementation
            {
              source = Path.v "src/demo.ml";
              outputs =
                [ Path.v "Demo.cmt"; Path.v "Demo.cmi"; Path.v "Demo.cmx" ];
              includes = [ Path.v "." ];
              flags = [];
            }
        in
        let write contents =
          Fs.write contents source |> Result.expect ~msg:"write source failed"
        in
        write "let value = 1\n";
        let first =
          Tusk_planner.Action_node.make ~actions:[ action ]
            ~outs:[ Path.v "Demo.cmt"; Path.v "Demo.cmi"; Path.v "Demo.cmx" ]
            ~srcs:[ Path.v "src/demo.ml" ] ~package ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "") ~deps:[]
        in
        write "let value = 2\n";
        let second =
          Tusk_planner.Action_node.make ~actions:[ action ]
            ~outs:[ Path.v "Demo.cmt"; Path.v "Demo.cmi"; Path.v "Demo.cmx" ]
            ~srcs:[ Path.v "src/demo.ml" ] ~package ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "") ~deps:[]
        in
        if Crypto.Hash.equal first.hash second.hash then
          Error
            "expected package-relative source edits to change the action hash"
        else Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_shared_library_links_stdlib_without_explicit_dependency () =
  match
    Fs.with_tempdir ~prefix:"planner_shared_stdlib" (fun tmpdir ->
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[] () in
        let store = Tusk_store.Store.create ~workspace in
        let package =
          {
            (make_package_with_paths ~name:"minttea"
               ~path:Path.(tmpdir / Path.v "packages" / Path.v "minttea")
               ~relative_path:(Path.v "packages/minttea"))
            with
            library = Some { path = Path.v "src/minttea.ml" };
          }
        in
        let ctx =
          Tusk_model.Build_ctx.make
            ~session_id:(Tusk_model.Session_id.of_string "test-session")
            ~profile:Tusk_model.Profile.release ()
        in
        let module_graph = G.make () in
        let _ =
          G.add_node module_graph
            (Tusk_planner.Module_node.make_library ~name:package.name
               ~includes:[ Path.v "." ])
        in
        let action_graph, _ =
          Tusk_planner.Action_graph.from_module_graph ~package
            ~profile:Tusk_model.Profile.release ~ctx ~toolchain:test_toolchain
            ~store ~depset:[] ~needs_unix:false ~needs_dynlink:false
            module_graph
        in
        match
          List.filter_map
            (function
              | Tusk_planner.Action.CreateSharedLibrary { libraries; _ } ->
                  Some libraries
              | _ -> None)
            (Tusk_planner.Action_graph.to_action_list action_graph)
        with
        | [ libraries ] ->
            if
              List.exists
                (fun library -> Path.equal library (Path.v "stdlib.cmxa"))
                libraries
            then Ok ()
            else Error "expected shared library link to include stdlib.cmxa"
        | [] -> Error "expected CreateSharedLibrary action"
        | _ -> Error "expected one CreateSharedLibrary action")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_shared_library_links_transitive_package_libraries () =
  match
    Fs.with_tempdir ~prefix:"planner_shared_dep_libs" (fun tmpdir ->
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[] () in
        let store = Tusk_store.Store.create ~workspace in
        let package =
          {
            (make_package_with_paths ~name:"tusk-eval"
               ~path:Path.(tmpdir / Path.v "packages" / Path.v "tusk-eval")
               ~relative_path:(Path.v "packages/tusk-eval"))
            with
            library = Some { path = Path.v "src/tusk_eval.ml" };
          }
        in
        let make_dep ~name ~artifact_dir ~depset =
          let dep_package =
            {
              (make_package_with_paths ~name
                 ~path:Path.(tmpdir / Path.v "packages" / Path.v name)
                 ~relative_path:Path.(Path.v "packages" / Path.v name))
              with
              library = Some { path = Path.v ("src/" ^ name ^ ".ml") };
            }
          in
          Tusk_planner.Dependency.
            {
              package = dep_package;
              artifact_dir;
              depset;
              hash = Crypto.hash_string name;
            }
        in
        let dep_c = make_dep ~name:"ceibo" ~artifact_dir:(Path.v "/cache/c") ~depset:[] in
        let dep_b =
          make_dep ~name:"syn" ~artifact_dir:(Path.v "/cache/b") ~depset:[ dep_c ]
        in
        let dep_a =
          make_dep ~name:"tusk-toolchain" ~artifact_dir:(Path.v "/cache/a")
            ~depset:[ dep_b ]
        in
        let ctx =
          Tusk_model.Build_ctx.make
            ~session_id:(Tusk_model.Session_id.of_string "test-session")
            ~profile:Tusk_model.Profile.release ()
        in
        let module_graph = G.make () in
        let _ =
          G.add_node module_graph
            (Tusk_planner.Module_node.make_library ~name:package.name
               ~includes:[ Path.v "." ])
        in
        let action_graph, _ =
          Tusk_planner.Action_graph.from_module_graph ~package
            ~profile:Tusk_model.Profile.release ~ctx ~toolchain:test_toolchain
            ~store ~depset:[ dep_a ] ~needs_unix:false ~needs_dynlink:false
            module_graph
        in
        match
          List.filter_map
            (function
              | Tusk_planner.Action.CreateSharedLibrary { libraries; _ } ->
                  Some libraries
              | _ -> None)
            (Tusk_planner.Action_graph.to_action_list action_graph)
        with
        | [ libraries ] ->
            let expected =
              [
                Path.v "stdlib.cmxa";
                Tusk_planner.Dependency.library_cmxa dep_c;
                Tusk_planner.Dependency.library_cmxa dep_b;
                Tusk_planner.Dependency.library_cmxa dep_a;
              ]
            in
            if libraries = expected then Ok ()
            else
              Error
                ("expected shared library dependencies ["
               ^ String.concat ", " (List.map Path.to_string expected)
               ^ "] but got ["
               ^ String.concat ", " (List.map Path.to_string libraries)
               ^ "]")
        | [] -> Error "expected CreateSharedLibrary action"
        | _ -> Error "expected one CreateSharedLibrary action")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_shared_library_adds_platform_linker_flags () =
  match
    Fs.with_tempdir ~prefix:"planner_shared_linker_flags" (fun tmpdir ->
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[] () in
        let store = Tusk_store.Store.create ~workspace in
        let package =
          {
            (make_package_with_paths ~name:"minttea"
               ~path:Path.(tmpdir / Path.v "packages" / Path.v "minttea")
               ~relative_path:(Path.v "packages/minttea"))
            with
            library = Some { path = Path.v "src/minttea.ml" };
          }
        in
        let ctx =
          Tusk_model.Build_ctx.make
            ~session_id:(Tusk_model.Session_id.of_string "test-session")
            ~profile:Tusk_model.Profile.release ()
        in
        let module_graph = G.make () in
        let _ =
          G.add_node module_graph
            (Tusk_planner.Module_node.make_library ~name:package.name
               ~includes:[ Path.v "." ])
        in
        let action_graph, _ =
          Tusk_planner.Action_graph.from_module_graph ~package
            ~profile:Tusk_model.Profile.release ~ctx ~toolchain:test_toolchain
            ~store ~depset:[] ~needs_unix:false ~needs_dynlink:false
            module_graph
        in
        let expected_flags =
          match Tusk_model.Build_ctx.target_platform_name ctx with
          | "macos" -> [ "-Wl,-undefined,dynamic_lookup" ]
          | _ -> []
        in
        match
          List.filter_map
            (function
              | Tusk_planner.Action.CreateSharedLibrary { cclib_flags; _ } ->
                  Some cclib_flags
              | _ -> None)
            (Tusk_planner.Action_graph.to_action_list action_graph)
        with
        | [ cclib_flags ] ->
            if cclib_flags = expected_flags then Ok ()
            else
              Error
                ("expected shared library linker flags ["
               ^ String.concat ", " expected_flags
               ^ "] but got ["
               ^ String.concat ", " cclib_flags ^ "]")
        | [] -> Error "expected CreateSharedLibrary action"
        | _ -> Error "expected one CreateSharedLibrary action")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.
    [
      case "action graph json round-trip preserves edges"
        test_action_graph_json_round_trip_preserves_dependencies;
      case "action graph json round-trip preserves package paths and hashes"
        test_action_graph_json_round_trip_preserves_package_paths_and_hashes;
      case "action hash tracks package-relative source contents"
        test_action_hash_tracks_package_relative_source_contents;
      case
        "shared libraries link stdlib without explicit stdlib dependency"
        test_shared_library_links_stdlib_without_explicit_dependency;
      case
        "shared libraries link transitive package libraries in dependency order"
        test_shared_library_links_transitive_package_libraries;
      case "shared libraries add platform linker flags"
        test_shared_library_adds_platform_linker_flags;
    ]

let name = "Planner Action Graph Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
