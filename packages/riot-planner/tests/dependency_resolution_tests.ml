open Std
open Std.Result.Syntax
open Riot_model
module Test = Std.Test
module G = Std.Graph.SimpleGraph

let make_package = fun name ->
  Riot_model.Package.make
    ~name:(Package_name.from_string name
    |> Result.expect ~msg:("expected valid package name: " ^ name))
    ~path:(Path.v ".")
    ~relative_path:(Path.v ".")
    ()

let make_src_graph_builder = fun ~package_root ~(package:Riot_model.Package.t) ~toolchain ~workspace ->
  let namespace =
    match package.library with
    | Some _ -> Namespace.empty
    | None -> Namespace.of_list [ Package.root_module_name package ]
  in
  Riot_planner.Module_graph.create
    Riot_planner.Module_graph.{
      root = package_root;
      source_groups =
        [ Riot_planner.Module_graph.{
            source_dir = Path.v "src";
            allowed_source_files = package.sources.src;
            root_mode =
              (
                match package.library with
                | Some _ -> Riot_planner.Module_graph.Library_root {
                  library_name = Package_name.to_string package.name
                }
                | None -> Riot_planner.Module_graph.Loose_sources
              );
            namespace;
          } ];
      package;
      toolchain;
      workspace;
    }

let test_transitive_closure_dependency_first_order = fun _ctx ->
  let dep_c =
    Riot_planner.Dependency.{
      package = make_package "c";
      artifact_dir = Path.v "/cache/c";
      depset = [];
      hash = Crypto.hash_string "c"
    } in
  let dep_b =
    Riot_planner.Dependency.{
      package = make_package "b";
      artifact_dir = Path.v "/cache/b";
      depset = [ dep_c ];
      hash = Crypto.hash_string "b"
    } in
  let dep_a =
    Riot_planner.Dependency.{
      package = make_package "a";
      artifact_dir = Path.v "/cache/a";
      depset = [ dep_b; dep_c ];
      hash = Crypto.hash_string "a"
    } in
  let names = Riot_planner.Dependency.transitive_closure [ dep_a ]
  |> List.map ~fn:(fun d -> d.Riot_planner.Dependency.package.name) in
  if
    names
    = List.map
      [ "c"; "b"; "a" ]
      ~fn:(fun value ->
        Package_name.from_string value
        |> Result.expect ~msg:("expected valid package name: " ^ value))
  then
    Ok ()
  else
    Error ("unexpected order: " ^ String.concat "," (List.map names ~fn:Package_name.to_string))

let test_library_cmxa_uses_store_location = fun _ctx ->
  let dep =
    Riot_planner.Dependency.{
      package = make_package "std";
      artifact_dir = Path.v "/tmp/cache/abcd";
      depset = [];
      hash = Crypto.hash_string "std"
    } in
  let expected =
    Path.(dep.artifact_dir
    / Riot_model.Module_name.(of_string (Package_name.to_string dep.package.name) |> cmxa)) in
  let got = Riot_planner.Dependency.library_cmxa dep in
  if Path.equal expected got then
    Ok ()
  else
    Error ("expected " ^ Path.to_string expected ^ " got " ^ Path.to_string got)

let test_module_graph_prefers_implementation_when_interface_exists = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_prefers_impl"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "type t\nval x : t\n" Path.(src_dir / Path.v "foo.mli")
        |> Result.expect ~msg:"expected foo.mli write to succeed" in
        let _ = Fs.write "type t = int\nlet x = 1\n" Path.(src_dir / Path.v "foo.ml")
        |> Result.expect ~msg:"expected foo.ml write to succeed" in
        let _ = Fs.write "val y : Foo.t\n" Path.(src_dir / Path.v "bar.mli") |> Result.expect ~msg:"expected bar.mli write to succeed" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "pkg"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg")
          ~sources:{
            src = [ Path.v "src/foo.mli"; Path.v "src/foo.ml"; Path.v "src/bar.mli" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let find_node_id expected_kind expected_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ when expected_kind = `implementation ->
                    String.equal
                      (Riot_model.Module.module_name mod_ |> Riot_model.Module_name.to_string)
                      expected_name
                | Riot_planner.Module_node.MLI mod_ when expected_kind = `interface ->
                    String.equal
                      (Riot_model.Module.module_name mod_ |> Riot_model.Module_name.to_string)
                      expected_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (node_id, _) -> Ok node_id
              | None -> Error ("expected node not found: " ^ expected_name)
            in
            match (
              find_node_id `interface "Bar",
              find_node_id `implementation "Foo",
              find_node_id `interface "Foo"
            ) with
            | Ok bar_mli_id, Ok foo_ml_id, Ok foo_mli_id -> (
                match G.get_node graph bar_mli_id with
                | None -> Error "expected bar.mli node to exist"
                | Some node ->
                    let depends_on_impl = List.any node.deps ~fn:(G.Node_id.eq foo_ml_id) in
                    let depends_on_intf = List.any node.deps ~fn:(G.Node_id.eq foo_mli_id) in
                    if depends_on_impl && not depends_on_intf then
                      Ok ()
                    else
                      Error "expected bar.mli to depend on foo.ml implementation node only"
              )
            | (Error msg, _, _)
            | (_, Error msg, _)
            | (_, _, Error msg) -> Error msg)
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_resolves_nested_local_unix_backend = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_nested_unix_backend"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let fs_dir = Path.(src_dir / Path.v "fs") in
        let fs_file_dir = Path.(fs_dir / Path.v "file") in
        let process_dir = Path.(src_dir / Path.v "process") in
        let env_dir = Path.(src_dir / Path.v "env") in
        let create_dir path = Fs.create_dir_all path
        |> Result.expect ~msg:("expected dir creation to succeed: " ^ Path.to_string path) in
        let write path contents = Fs.write contents path
        |> Result.expect ~msg:("expected file write to succeed: " ^ Path.to_string path) in
        let _ = create_dir fs_file_dir in
        let _ = create_dir process_dir in
        let _ = create_dir env_dir in
        let _ = write Path.(src_dir / Path.v "kernel_new.ml") "module Fs = Fs\nmodule Process = Process\nmodule Env = Env\n" in
        let _ = write Path.(fs_dir / Path.v "fs.ml") "module File = File\n" in
        let _ = write Path.(fs_dir / Path.v "fs.mli") "module File : sig type t end\n" in
        let _ = write Path.(fs_file_dir / Path.v "file.ml") "include Unix\n" in
        let _ = write Path.(fs_file_dir / Path.v "file.mli") "type t\n" in
        let _ = write Path.(fs_file_dir / Path.v "unix.ml") "type t = int\n" in
        let _ = write Path.(process_dir / Path.v "process.ml") "include Unix\n" in
        let _ = write Path.(process_dir / Path.v "unix.ml") "let inherited : Fs.File.t option = None\n" in
        let _ = write Path.(env_dir / Path.v "env.ml") "include Unix\n" in
        let _ = write Path.(env_dir / Path.v "unix.ml") "let cwd = \".\"\n" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "kernel-new"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg")
          ~sources:{
            src =
              [
                Path.v "src/kernel_new.ml";
                Path.v "src/fs/fs.ml";
                Path.v "src/fs/fs.mli";
                Path.v "src/fs/file/file.ml";
                Path.v "src/fs/file/file.mli";
                Path.v "src/fs/file/unix.ml";
                Path.v "src/process/process.ml";
                Path.v "src/process/unix.ml";
                Path.v "src/env/env.ml";
                Path.v "src/env/unix.ml";
              ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let find_ml qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            match (
              find_ml "Kernel_new__Fs__File",
              find_ml "Kernel_new__Fs__File__Unix",
              find_ml "Kernel_new__Process__Unix",
              G.topo_sort graph
            ) with
            | Ok file_node, Ok file_unix_node, Ok process_unix_node, Ok _ ->
                let depends_on_file_unix = List.any
                  file_node.deps
                  ~fn:(G.Node_id.eq file_unix_node.id) in
                let depends_on_process_unix = List.any
                  file_node.deps
                  ~fn:(G.Node_id.eq process_unix_node.id) in
                if depends_on_file_unix && not depends_on_process_unix then
                  Ok ()
                else
                  Error "expected Fs.File.File to depend on Fs.File.Unix only"
            | (Error msg, _, _, _)
            | (_, Error msg, _, _)
            | (_, _, Error msg, _) ->
                Error msg
            | _, _, _, Error cycle_ids ->
                Error ("unexpected cycle: "
                ^ String.concat " -> " (List.map cycle_ids ~fn:G.Node_id.to_string)))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_uses_explicit_root_library_path = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_explicit_root_library_path"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "let value = Helper.value\n" Path.(src_dir / Path.v "lib.ml")
        |> Result.expect ~msg:"expected lib.ml write to succeed" in
        let _ = Fs.write "let value = 42\n" Path.(src_dir / Path.v "helper.ml")
        |> Result.expect ~msg:"expected helper.ml write to succeed" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "pkg"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg") ~library:{ path = Path.v "src/lib.ml" }
          ~sources:{
            src = [ Path.v "src/lib.ml"; Path.v "src/helper.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () -> (
            let graph = Riot_planner.Module_graph.graph graph_builder in
            match G.topo_sort graph with
            | Error cycle_ids -> Error ("unexpected cycle: "
            ^ String.concat " -> " (List.map cycle_ids ~fn:G.Node_id.to_string))
            | Ok _ ->
                let impl_nodes =
                  G.map graph ~fn:(fun x -> x)
                  |> List.filter_map
                    ~fn:(fun (_id, (node: Riot_planner.Module_node.t G.node)) ->
                      match node.value.kind with
                      | Riot_planner.Module_node.ML mod_ -> Some (mod_, node.value.file)
                      | _ -> None)
                in
                let has_pkg_root =
                  List.any impl_nodes
                    ~fn:(fun (mod_, file) ->
                      String.equal
                        (Riot_model.Module_name.to_string (Riot_model.Module.module_name mod_))
                        "Pkg"
                      && match file with
                      | Riot_planner.Module_node.Concrete path -> Path.equal
                        path
                        (Path.v "src/lib.ml")
                      | _ -> false)
                in
                let has_child_lib =
                  List.any impl_nodes
                    ~fn:(fun (mod_, _file) ->
                      String.equal (Riot_model.Module.namespaced_name mod_) "Pkg__Lib")
                in
                if has_pkg_root && not has_child_lib then
                  Ok ()
                else if not has_pkg_root then
                  Error "expected package root implementation to use src/lib.ml"
                else
                  Error "did not expect src/lib.ml to also appear as child module Pkg__Lib"
          ))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_uses_explicit_root_library_path_case_insensitively = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_explicit_root_library_path_case_insensitive"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "let value = Helper.value\n" Path.(src_dir / Path.v "Krasny.ml")
        |> Result.expect ~msg:"expected Krasny.ml write to succeed" in
        let _ = Fs.write "let value = 42\n" Path.(src_dir / Path.v "helper.ml")
        |> Result.expect ~msg:"expected helper.ml write to succeed" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "krasny"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg") ~library:{ path = Path.v "src/krasny.ml" }
          ~sources:{
            src = [ Path.v "src/Krasny.ml"; Path.v "src/helper.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () -> (
            let graph = Riot_planner.Module_graph.graph graph_builder in
            match G.topo_sort graph with
            | Error cycle_ids -> Error ("unexpected cycle: "
            ^ String.concat " -> " (List.map cycle_ids ~fn:G.Node_id.to_string))
            | Ok _ ->
                let impl_nodes =
                  G.map graph ~fn:(fun x -> x)
                  |> List.filter_map
                    ~fn:(fun (_id, (node: Riot_planner.Module_node.t G.node)) ->
                      match node.value.kind with
                      | Riot_planner.Module_node.ML mod_ -> Some (mod_, node.value.file)
                      | _ -> None)
                in
                let has_pkg_root =
                  List.any impl_nodes
                    ~fn:(fun (mod_, file) ->
                      String.equal
                        (Riot_model.Module_name.to_string (Riot_model.Module.module_name mod_))
                        "Krasny"
                      && match file with
                      | Riot_planner.Module_node.Concrete path -> Path.equal
                        path
                        (Path.v "src/Krasny.ml")
                      | _ -> false)
                in
                let has_child_krasny =
                  List.any impl_nodes
                    ~fn:(fun (mod_, _file) ->
                      String.equal (Riot_model.Module.namespaced_name mod_) "Krasny__Krasny")
                in
                if has_pkg_root && not has_child_krasny then
                  Ok ()
                else if not has_pkg_root then
                  Error "expected package root implementation to use src/Krasny.ml despite explicit path case mismatch"
                else
                  Error "did not expect src/Krasny.ml to also appear as child module Krasny__Krasny"
          ))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_root_library_alias_depends_on_child_module = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_root_library_alias_depends_on_child_module"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "module A = A\n" Path.(src_dir / Path.v "lib_with_deps.ml")
        |> Result.expect ~msg:"expected lib_with_deps.ml write to succeed" in
        let _ = Fs.write "let value = 42\n" Path.(src_dir / Path.v "a.ml") |> Result.expect ~msg:"expected a.ml write to succeed" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "lib_with_deps"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg") ~library:{ path = Path.v "src/lib_with_deps.ml" }
          ~sources:{
            src = [ Path.v "src/lib_with_deps.ml"; Path.v "src/a.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let find_ml qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            match (find_ml "Lib_with_deps", find_ml "Lib_with_deps__A") with
            | Ok root_node, Ok a_node ->
                if List.any root_node.deps ~fn:(G.Node_id.eq a_node.id) then
                  Ok ()
                else
                  Error "expected Lib_with_deps root module to depend on Lib_with_deps__A"
            | (Error msg, _)
            | (_, Error msg) -> Error msg)
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_opened_public_root_resolves_children_to_public_module = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_opened_public_root"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "module Token = Token\n" Path.(src_dir / Path.v "syn.ml")
        |> Result.expect ~msg:"expected syn.ml write to succeed" in
        let _ = Fs.write "let value = 42\n" Path.(src_dir / Path.v "token.ml")
        |> Result.expect ~msg:"expected token.ml write to succeed" in
        let _ = Fs.write "open Syn\nlet _ = Token.value\n" Path.(src_dir / Path.v "main.ml")
        |> Result.expect ~msg:"expected main.ml write to succeed" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "syn"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg") ~library:{ path = Path.v "src/syn.ml" } ~binaries:[
          Riot_model.Package.{ name = "syn"; path = Path.v "src/main.ml" }
        ]
          ~sources:{
            src = [ Path.v "src/syn.ml"; Path.v "src/token.ml"; Path.v "src/main.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let analyzed_modules = Riot_planner.Module_graph.analyzed_modules graph_builder in
            let find_ml qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            let find_analyzed_module path =
              let expected_suffix = "/" ^ path in
              List.find analyzed_modules
                ~fn:(fun (_id, analyzed) ->
                  let display_path = Path.to_string analyzed.display_path in
                  String.equal display_path path || String.ends_with ~suffix:expected_suffix display_path) |> Option.map
                ~fn:(fun (_id, analyzed) -> analyzed)
            in
            match (
              find_ml "Syn",
              find_ml "Syn__Token",
              find_ml "Syn__Main",
              find_analyzed_module "src/main.ml"
            ) with
            | Ok syn_node, Ok token_node, Ok main_node, Some analyzed_main -> (
                match analyzed_main.deps with
                | Error err ->
                    Error (
                      "dependency analysis failed: " ^ (
                        match err with
                        | Syn.Deps.Parse_diagnostics diagnostics -> String.concat
                          "; "
                          (List.map diagnostics ~fn:Syn.Diagnostic.to_string)
                        | Syn.Deps.Cst_builder_error build_err -> build_err.message
                      )
                    )
                | Ok deps ->
                    let modules = Syn.Deps.modules deps in
                    let depends_on_syn = List.any main_node.deps ~fn:(G.Node_id.eq syn_node.id) in
                    let depends_on_token = List.any main_node.deps ~fn:(G.Node_id.eq token_node.id) in
                    if modules = [ "Syn" ] && depends_on_syn && not depends_on_token then
                      Ok ()
                    else
                      Error ("expected opened child reference to resolve through public root only; modules=["
                      ^ String.concat ", " modules
                      ^ "], depends_on_syn="
                      ^ Bool.to_string depends_on_syn
                      ^ ", depends_on_token="
                      ^ Bool.to_string depends_on_token
                      ^ ")")
              )
            | (Error msg, _, _, _)
            | (_, Error msg, _, _)
            | (_, _, Error msg, _) ->
                Error msg
            | _ ->
                Error "expected analyzed main module to exist")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_implicit_alias_opens_resolve_nested_leaf_modules = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_implicit_alias_opens_nested_leaf_modules"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let net_addr_dir = Path.(src_dir / Path.v "net/addr") in
        let create_dir path = Fs.create_dir_all path
        |> Result.expect ~msg:("expected dir creation to succeed: " ^ Path.to_string path) in
        let write path contents = Fs.write contents path
        |> Result.expect ~msg:("expected file write to succeed: " ^ Path.to_string path) in
        let _ = create_dir net_addr_dir in
        let _ = write Path.(src_dir / Path.v "kernel.ml") "module Net = Net\nmodule Result = Result\nmodule SystemError = System_error\n" in
        let _ = write Path.(src_dir / Path.v "result.mli") "type ('value, 'error) t = ('value, 'error) result = | Ok of 'value | Error of 'error\n" in
        let _ = write Path.(src_dir / Path.v "result.ml") "type ('value, 'error) t = ('value, 'error) result = | Ok of 'value | Error of 'error\n" in
        let _ = write Path.(src_dir / Path.v "system_error.mli") "type t = Unknown\n" in
        let _ = write Path.(src_dir / Path.v "system_error.ml") "type t = Unknown\n" in
        let _ = write Path.(src_dir / Path.v "net/net.ml") "module Addr = Addr\nmodule Socket_addr = Socket_addr\n" in
        let _ = write Path.(src_dir / Path.v "net/socket_addr.mli") "type t\n" in
        let _ = write Path.(src_dir / Path.v "net/socket_addr.ml") "type t = int\n" in
        let _ = write Path.(src_dir / Path.v "net/addr/addr.ml") "module Unix = Unix\n" in
        let _ = write Path.(src_dir / Path.v "net/addr/unix.mli") "type error =\n  | System of System_error.t\nval resolve_stream: host:string -> port:int -> (Socket_addr.t array, error) Result.t\n" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "kernel"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg") ~library:{ path = Path.v "src/kernel.ml" }
          ~sources:{
            src =
              [
                Path.v "src/kernel.ml";
                Path.v "src/result.mli";
                Path.v "src/result.ml";
                Path.v "src/system_error.mli";
                Path.v "src/system_error.ml";
                Path.v "src/net/net.ml";
                Path.v "src/net/socket_addr.mli";
                Path.v "src/net/socket_addr.ml";
                Path.v "src/net/addr/addr.ml";
                Path.v "src/net/addr/unix.mli";
              ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let analyzed_modules = Riot_planner.Module_graph.analyzed_modules graph_builder in
            let find_ml qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            let find_mli qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.MLI mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            let find_analyzed_module path =
              let expected_suffix = "/" ^ path in
              List.find analyzed_modules
                ~fn:(fun (_id, analyzed) ->
                  let display_path = Path.to_string analyzed.display_path in
                  String.equal display_path path || String.ends_with ~suffix:expected_suffix display_path) |> Option.map
                ~fn:(fun (_id, analyzed) -> analyzed)
            in
            match (
              find_mli "Kernel__Net__Addr__Unix",
              find_ml "Kernel__Result",
              find_ml "Kernel__System_error",
              find_ml "Kernel__Net__Socket_addr",
              find_analyzed_module "src/net/addr/unix.mli"
            ) with
            | Ok unix_node, Ok result_node, Ok system_error_node, Ok socket_addr_node, Some analyzed_unix -> (
                match analyzed_unix.deps with
                | Error err ->
                    Error (
                      "dependency analysis failed: " ^ (
                        match err with
                        | Syn.Deps.Parse_diagnostics diagnostics -> String.concat
                          "; "
                          (List.map diagnostics ~fn:Syn.Diagnostic.to_string)
                        | Syn.Deps.Cst_builder_error build_err -> build_err.message
                      )
                    )
                | Ok deps ->
                    let modules = Syn.Deps.modules deps in
                    let depends_on_result = List.any unix_node.deps ~fn:(G.Node_id.eq result_node.id) in
                    let depends_on_system_error = List.any
                      unix_node.deps
                      ~fn:(G.Node_id.eq system_error_node.id) in
                    let depends_on_socket_addr = List.any
                      unix_node.deps
                      ~fn:(G.Node_id.eq socket_addr_node.id) in
                    if
                      modules = [ "Result"; "Socket_addr"; "System_error" ]
                      && depends_on_result
                      && depends_on_system_error
                      && depends_on_socket_addr
                    then
                      Ok ()
                    else
                      Error ("expected implicit alias opens to resolve nested leaf modules; modules=["
                      ^ String.concat ", " modules
                      ^ "], depends_on_result="
                      ^ Bool.to_string depends_on_result
                      ^ ", depends_on_system_error="
                      ^ Bool.to_string depends_on_system_error
                      ^ ", depends_on_socket_addr="
                      ^ Bool.to_string depends_on_socket_addr
                      ^ ")")
              )
            | (Error msg, _, _, _, _)
            | (_, Error msg, _, _, _)
            | (_, _, Error msg, _, _)
            | (_, _, _, Error msg, _) ->
                Error msg
            | _ ->
                Error "expected analyzed unix.mli module to exist")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_implicit_root_alias_resolves_public_child_root = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_implicit_root_alias_public_child_root"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let fs_file_dir = Path.(src_dir / Path.v "fs/file") in
        let process_dir = Path.(src_dir / Path.v "process") in
        let create_dir path = Fs.create_dir_all path
        |> Result.expect ~msg:("expected dir creation to succeed: " ^ Path.to_string path) in
        let write path contents = Fs.write contents path
        |> Result.expect ~msg:("expected file write to succeed: " ^ Path.to_string path) in
        let _ = create_dir fs_file_dir in
        let _ = create_dir process_dir in
        let _ = write Path.(src_dir / Path.v "kernel.ml") "module Fs = Fs\nmodule Process = Process\nmodule SystemError = System_error\n" in
        let _ = write Path.(src_dir / Path.v "system_error.mli") "type t = Unknown\n" in
        let _ = write Path.(src_dir / Path.v "system_error.ml") "type t = Unknown\n" in
        let _ = write Path.(src_dir / Path.v "fs/fs.ml") "module File = File\n" in
        let _ = write Path.(src_dir / Path.v "fs/file/file.mli") "type t\ntype error\n" in
        let _ = write Path.(src_dir / Path.v "fs/file/file.ml") "type t = int\ntype error = unit\n" in
        let _ = write Path.(src_dir / Path.v "process/process.mli") "type error =\n  | File of Fs.File.error\n  | System of System_error.t\n" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "kernel"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg") ~library:{ path = Path.v "src/kernel.ml" }
          ~sources:{
            src = [
              Path.v "src/kernel.ml";
              Path.v "src/system_error.mli";
              Path.v "src/system_error.ml";
              Path.v "src/fs/fs.ml";
              Path.v "src/fs/file/file.mli";
              Path.v "src/fs/file/file.ml";
              Path.v "src/process/process.mli";
            ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let analyzed_modules = Riot_planner.Module_graph.analyzed_modules graph_builder in
            let find_ml qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            let find_mli qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.MLI mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            let find_analyzed_module path =
              let expected_suffix = "/" ^ path in
              List.find analyzed_modules
                ~fn:(fun (_id, analyzed) ->
                  let display_path = Path.to_string analyzed.display_path in
                  String.equal display_path path || String.ends_with ~suffix:expected_suffix display_path) |> Option.map
                ~fn:(fun (_id, analyzed) -> analyzed)
            in
            match (
              find_mli "Kernel__Process",
              find_ml "Kernel__Fs",
              find_ml "Kernel__Fs__File",
              find_ml "Kernel__System_error",
              find_analyzed_module "src/process/process.mli"
            ) with
            | Ok process_node, Ok fs_node, Ok fs_file_node, Ok system_error_node, Some analyzed_process -> (
                match analyzed_process.deps with
                | Error err ->
                    Error (
                      "dependency analysis failed: " ^ (
                        match err with
                        | Syn.Deps.Parse_diagnostics diagnostics -> String.concat
                          "; "
                          (List.map diagnostics ~fn:Syn.Diagnostic.to_string)
                        | Syn.Deps.Cst_builder_error build_err -> build_err.message
                      )
                    )
                | Ok deps ->
                    let modules = Syn.Deps.modules deps in
                    let depends_on_fs = List.any process_node.deps ~fn:(G.Node_id.eq fs_node.id) in
                    let depends_on_fs_file = List.any
                      process_node.deps
                      ~fn:(G.Node_id.eq fs_file_node.id) in
                    let depends_on_system_error = List.any
                      process_node.deps
                      ~fn:(G.Node_id.eq system_error_node.id) in
                    if
                      modules = [ "Fs"; "System_error" ]
                      && depends_on_fs
                      && not depends_on_fs_file
                      && depends_on_system_error
                    then
                      Ok ()
                    else
                      Error ("expected implicit root alias to resolve through public child root; modules=["
                      ^ String.concat ", " modules
                      ^ "], depends_on_fs="
                      ^ Bool.to_string depends_on_fs
                      ^ ", depends_on_fs_file="
                      ^ Bool.to_string depends_on_fs_file
                      ^ ", depends_on_system_error="
                      ^ Bool.to_string depends_on_system_error
                      ^ ")")
              )
            | (Error msg, _, _, _, _)
            | (_, Error msg, _, _, _)
            | (_, _, Error msg, _, _)
            | (_, _, _, Error msg, _) ->
                Error msg
            | _ ->
                Error "expected analyzed process.mli module to exist")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_resolves_deeply_nested_modules_namespace_first = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_deeply_nested_modules"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let testing_dir = Path.(src_dir / Path.v "domains/admin/users/models/testing") in
        let models_dir = Path.(src_dir / Path.v "domains/admin/users/models") in
        let admin_dir = Path.(src_dir / Path.v "domains/admin") in
        let create_dir path = Fs.create_dir_all path
        |> Result.expect ~msg:("expected dir creation to succeed: " ^ Path.to_string path) in
        let write path contents = Fs.write contents path
        |> Result.expect ~msg:("expected file write to succeed: " ^ Path.to_string path) in
        let _ = create_dir testing_dir in
        let _ = create_dir models_dir in
        let _ = create_dir admin_dir in
        let _ = write Path.(src_dir / Path.v "shared.ml") "let level = \"root\"\n" in
        let _ = write Path.(src_dir / Path.v "helpers.ml") "let level = \"root\"\n" in
        let _ = write Path.(admin_dir / Path.v "shared.ml") "let level = \"admin\"\n" in
        let _ = write Path.(models_dir / Path.v "helpers.ml") "let level = \"models\"\n" in
        let _ = write Path.(testing_dir / Path.v "shared.ml") "let level = \"testing\"\n" in
        let _ = write Path.(testing_dir / Path.v "user.ml") "include Shared\n" in
        let _ = write Path.(testing_dir / Path.v "report.ml") "include Helpers\n" in
        let package = Riot_model.Package.make ~name:(Package_name.from_string "deep-graph"
        |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
          "pkg")
          ~sources:{
            src = [
              Path.v "src/shared.ml";
              Path.v "src/helpers.ml";
              Path.v "src/domains/admin/shared.ml";
              Path.v "src/domains/admin/users/models/helpers.ml";
              Path.v "src/domains/admin/users/models/testing/shared.ml";
              Path.v "src/domains/admin/users/models/testing/user.ml";
              Path.v "src/domains/admin/users/models/testing/report.ml";
            ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:"target"
          () in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let find_ml qualified_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ -> String.equal
                  (Riot_model.Module.namespaced_name mod_)
                  qualified_name
                | _ -> false
              in
              match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
              | Some (_node_id, node) -> Ok node
              | None -> Error ("expected module not found: " ^ qualified_name)
            in
            let depends_on (node: Riot_planner.Module_node.t G.node) (
              dependency: Riot_planner.Module_node.t G.node
            ) = List.any node.deps ~fn:(G.Node_id.eq dependency.id) in
            match (
              find_ml "Deep_graph__Domains__Admin__Users__Models__Testing__User",
              find_ml "Deep_graph__Domains__Admin__Users__Models__Testing__Report",
              find_ml "Deep_graph__Domains__Admin__Users__Models__Testing__Shared",
              find_ml "Deep_graph__Domains__Admin__Shared",
              find_ml "Deep_graph__Shared",
              find_ml "Deep_graph__Domains__Admin__Users__Models__Helpers",
              find_ml "Deep_graph__Helpers",
              G.topo_sort graph
            ) with
            | Ok user_node, Ok report_node, Ok testing_shared_node, Ok admin_shared_node, Ok root_shared_node, Ok models_helpers_node, Ok root_helpers_node, Ok _ ->
                let user_depends_on_testing_shared = depends_on user_node testing_shared_node in
                let user_depends_on_admin_shared = depends_on user_node admin_shared_node in
                let user_depends_on_root_shared = depends_on user_node root_shared_node in
                let report_depends_on_models_helpers = depends_on report_node models_helpers_node in
                let report_depends_on_root_helpers = depends_on report_node root_helpers_node in
                if
                  user_depends_on_testing_shared
                  && not user_depends_on_admin_shared
                  && not user_depends_on_root_shared
                  && report_depends_on_models_helpers
                  && not report_depends_on_root_helpers
                then
                  Ok ()
                else
                  Error "expected deep modules to prefer local and nearest ancestor namespaces"
            | (Error msg, _, _, _, _, _, _, _)
            | (_, Error msg, _, _, _, _, _, _)
            | (_, _, Error msg, _, _, _, _, _)
            | (_, _, _, Error msg, _, _, _, _)
            | (_, _, _, _, Error msg, _, _, _)
            | (_, _, _, _, _, Error msg, _, _)
            | (_, _, _, _, _, _, Error msg, _) ->
                Error msg
            | _, _, _, _, _, _, _, Error cycle_ids ->
                Error ("unexpected cycle: "
                ^ String.concat " -> " (List.map cycle_ids ~fn:G.Node_id.to_string)))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_module_graph_keeps_nested_sibling_dependency_across_allowed_source_orders = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_nested_udp_order"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let net_dir = Path.(src_dir / Path.v "net") in
        let create_dir path = Fs.create_dir_all path
        |> Result.expect ~msg:("expected dir creation to succeed: " ^ Path.to_string path) in
        let write path contents = Fs.write contents path
        |> Result.expect ~msg:("expected file write to succeed: " ^ Path.to_string path) in
        let _ = create_dir net_dir in
        let _ = write Path.(src_dir / Path.v "demo.ml") "module Net = Net\n" in
        let _ = write Path.(net_dir / Path.v "net.ml") "module Udp_socket = Udp_socket\nmodule Udp_server = Udp_server\n" in
        let _ = write Path.(net_dir / Path.v "udp_socket.mli") "type t\n" in
        let _ = write Path.(net_dir / Path.v "udp_socket.ml") "type t = unit\n" in
        let _ = write Path.(net_dir / Path.v "udp_server.mli") "type handler = socket:Udp_socket.t -> bytes -> unit\nval run : handler -> unit\n" in
        let _ = write Path.(net_dir / Path.v "udp_server.ml") "type handler = socket:Udp_socket.t -> bytes -> unit\nlet run _ = ()\n" in
        let source_orders = [
          [
            Path.v "src/demo.ml";
            Path.v "src/net/net.ml";
            Path.v "src/net/udp_socket.mli";
            Path.v "src/net/udp_socket.ml";
            Path.v "src/net/udp_server.mli";
            Path.v "src/net/udp_server.ml";
          ];
          [
            Path.v "src/net/udp_server.mli";
            Path.v "src/net/udp_server.ml";
            Path.v "src/net/udp_socket.mli";
            Path.v "src/net/udp_socket.ml";
            Path.v "src/net/net.ml";
            Path.v "src/demo.ml";
          ];
          [
            Path.v "src/net/udp_socket.ml";
            Path.v "src/net/net.ml";
            Path.v "src/demo.ml";
            Path.v "src/net/udp_server.ml";
            Path.v "src/net/udp_socket.mli";
            Path.v "src/net/udp_server.mli";
          ];
          [
            Path.v "src/net/net.ml";
            Path.v "src/net/udp_server.mli";
            Path.v "src/demo.ml";
            Path.v "src/net/udp_socket.mli";
            Path.v "src/net/udp_server.ml";
            Path.v "src/net/udp_socket.ml";
          ];
        ] in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let source_order_to_string source_order = source_order
        |> List.map ~fn:Path.to_string
        |> String.concat ", " in
        let rec run = function
          | [] -> Ok ()
          | source_order :: rest ->
              let package = Riot_model.Package.make ~name:(Package_name.from_string "demo"
              |> Result.expect ~msg:"expected valid package name") ~path:package_root ~relative_path:(Path.v
                "pkg") ~library:{ path = Path.v "src/demo.ml" }
                ~sources:{
                  src = source_order;
                  native = [];
                  tests = [];
                  examples = [];
                  bench = [];
                }
                ()
              in
              let workspace = Riot_model.Workspace.make_realized
                ~root:tmpdir
                ~packages:[ package ]
                ~target_dir:"target"
                () in
              let graph_builder = make_src_graph_builder ~package_root ~package ~toolchain ~workspace in
              match Riot_planner.Module_graph.wire_dependencies graph_builder with
              | Error err -> Error ("unexpected planner error for allowed source order ["
              ^ source_order_to_string source_order
              ^ "]: "
              ^ Riot_planner.Planning_error.to_string err)
              | Ok () ->
                  let graph = Riot_planner.Module_graph.graph graph_builder in
                  let find_mli qualified_name =
                    let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                      match node.value.kind with
                      | Riot_planner.Module_node.MLI mod_ -> String.equal
                        (Riot_model.Module.namespaced_name mod_)
                        qualified_name
                      | _ -> false
                    in
                    match List.find (G.map graph ~fn:(fun x -> x)) ~fn:matches with
                    | Some (_node_id, node) -> Ok node
                    | None -> Error ("expected module not found: " ^ qualified_name)
                  in
                  let find_modules qualified_name =
                    let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                      match node.value.kind with
                      | Riot_planner.Module_node.ML mod_
                      | Riot_planner.Module_node.MLI mod_ -> String.equal
                        (Riot_model.Module.namespaced_name mod_)
                        qualified_name
                      | _ -> false
                    in
                    let matches =
                      List.filter (G.map graph ~fn:(fun x -> x)) ~fn:matches
                    in
                    if List.is_empty matches then
                      Error ("expected module not found: " ^ qualified_name)
                    else
                      Ok (List.map matches ~fn:(fun (_node_id, node) -> node))
                  in
                  let module_dependency_labels ((node: Riot_planner.Module_node.t G.node)) =
                    List.filter_map node.deps
                      ~fn:(fun dep_id ->
                        match G.get_node graph dep_id with
                        | Some dep_node -> (
                            match dep_node.value.kind with
                            | Riot_planner.Module_node.ML mod_ -> Some ("ML("
                            ^ Riot_model.Module.namespaced_name mod_
                            ^ ")")
                            | Riot_planner.Module_node.MLI mod_ -> Some ("MLI("
                            ^ Riot_model.Module.namespaced_name mod_
                            ^ ")")
                            | Riot_planner.Module_node.Library _ -> Some "Library"
                            | Riot_planner.Module_node.Binary _ -> Some "Binary"
                            | Riot_planner.Module_node.C -> Some "C"
                            | Riot_planner.Module_node.H -> Some "H"
                            | Riot_planner.Module_node.Native _ -> Some "Native"
                            | Riot_planner.Module_node.Other label -> Some ("Other(" ^ label ^ ")")
                            | Riot_planner.Module_node.Root -> Some "Root"
                          )
                        | None -> None)
                  in
                  match (
                    find_mli "Demo__Net__Udp_server",
                    find_modules "Demo__Net__Udp_socket",
                    G.topo_sort graph
                  ) with
                  | Ok udp_server_mli, Ok udp_socket_nodes, Ok _ ->
                      if
                        List.any
                          udp_socket_nodes
                          ~fn:(fun udp_socket_node ->
                            List.any udp_server_mli.deps ~fn:(G.Node_id.eq udp_socket_node.id))
                      then
                        run rest
                      else
                        Error ("expected Demo__Net__Udp_server to depend on Demo__Net__Udp_socket for allowed source order ["
                        ^ source_order_to_string source_order
                        ^ "], got deps ["
                        ^ String.concat ", " (module_dependency_labels udp_server_mli)
                        ^ "]")
                  | (Error msg, _, _)
                  | (_, Error msg, _) -> Error msg
                  | _, _, Error cycle_ids -> Error ("unexpected cycle: "
                  ^ String.concat " -> " (List.map cycle_ids ~fn:G.Node_id.to_string))
        in
        run source_orders)
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.[
    case "transitive closure order and dedup" test_transitive_closure_dependency_first_order;
    case "library cmxa path from artifact_dir" test_library_cmxa_uses_store_location;
    case "module graph prefers implementation when interface exists" test_module_graph_prefers_implementation_when_interface_exists;
    case "module graph uses explicit root library path" test_module_graph_uses_explicit_root_library_path;
    case "module graph uses explicit root library path despite case mismatch" test_module_graph_uses_explicit_root_library_path_case_insensitively;
    case "module graph root library alias depends on child module" test_module_graph_root_library_alias_depends_on_child_module;
    case "module graph opened public root resolves children to public module" test_module_graph_opened_public_root_resolves_children_to_public_module;
    case "module graph implicit alias opens resolve nested leaf modules" test_module_graph_implicit_alias_opens_resolve_nested_leaf_modules;
    case "module graph implicit root alias resolves public child root" test_module_graph_implicit_root_alias_resolves_public_child_root;
    case "module graph resolves nested local unix backend" test_module_graph_resolves_nested_local_unix_backend;
    case "module graph resolves deeply nested modules namespace-first" test_module_graph_resolves_deeply_nested_modules_namespace_first;
    case "module graph keeps nested sibling dependency across allowed source orders" test_module_graph_keeps_nested_sibling_dependency_across_allowed_source_orders;
  ]

let name = "Planner Dependency Resolution Tests"

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
