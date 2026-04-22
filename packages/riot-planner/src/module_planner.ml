(** Build Planner - Orchestrates build graph creation, wiring, and action
    generation *)
open Std
open Std.Collections
open Riot_model
module G = Graph.SimpleGraph

type plan_input = {
  package: Package.t;
  profile: Profile.t;
  ctx: Build_ctx.t;
  toolchain: Riot_toolchain.t;
  workspace: Workspace.t;
  source_groups: Module_graph.source_group list;
  depset: Dependency.t list;
  store: Riot_store.Store.t;
}

type plan_result = {
  sources: Path.t list;
  module_graph: Module_node.t G.t;
  analyzed_modules: (G.Node_id.t * Module_graph.analyzed_module) list;
  action_graph: Action_graph.t;
}

let plan_node = fun (input: plan_input) ->
  let config =
    Module_graph.{
      root = input.package.path;
      source_groups = input.source_groups;
      package = input.package;
      toolchain = input.toolchain;
      workspace = input.workspace;
    }
  in
  try
    let graph_builder = Module_graph.create config in
    (
      match input.package.sources.native with
      | [] -> ()
      | files ->
          let native_node = Module_node.make_native ~files in
          let _ = G.add_node (Module_graph.graph graph_builder) native_node in
          ()
    );
    match Module_graph.wire_dependencies graph_builder with
    | Error _ as error -> error
    | Ok () ->
        (
          match input.package.library with
          | Some _lib -> Module_graph.add_library_node
            graph_builder
            ~name:(Package_name.to_string input.package.name)
            ~includes:[]
          | None -> ()
        );
        (* Direct deps are used for linking, but compile-time includes need the
           full transitive closure because package alias/interface modules can
           reference transitive package modules. *)
        let all_deps = input.depset in
        let transitive_deps = Dependency.transitive_closure all_deps in
        (* Check if any package (including our own) needs unix *)
        let needs_unix =
          let check_pkg (pkg: Package.t) =
            List.any (Package.build_graph_dependencies pkg)
              ~fn:(fun (d: Package.dependency) ->
                Package_name.equal d.name
                  (Package_name.from_string "unix" |> Result.expect ~msg:"expected valid package name"))
          in
          check_pkg input.package
          || List.any transitive_deps ~fn:(fun (dep: Dependency.t) -> check_pkg dep.package)
        in
        (* Check if any package (including our own) needs dynlink *)
        let needs_dynlink =
          let check_pkg (pkg: Package.t) =
            List.any (Package.build_graph_dependencies pkg)
              ~fn:(fun (d: Package.dependency) ->
                Package_name.equal d.name
                  (Package_name.from_string "dynlink" |> Result.expect ~msg:"expected valid package name"))
          in
          check_pkg input.package
          || List.any transitive_deps ~fn:(fun (dep: Dependency.t) -> check_pkg dep.package)
        in
        let binary_libraries =
          (* Binaries and commands need the full transitive runtime library
             closure, not just direct deps, because a direct package library
             like Std can reference modules provided by transitive deps such as
             Kernel and Actors. Dev-only packages have no local library, but
             their test/example binaries still need the dependency closure. *)
          let unix_lib =
            if needs_unix then
              [ Path.v "unix.cmxa" ]
            else
              []
          in
          let dynlink_lib =
            if needs_dynlink then
              [ Path.v "dynlink.cmxa" ]
            else
              []
          in
          let dep_libs = List.map transitive_deps ~fn:Dependency.library_cmxa in
          let own_lib =
            match input.package.library with
            | Some _ -> [
              Module_name.(of_string (Package_name.to_string input.package.name) |> cmxa)
            ]
            | None -> []
          in
          let seen_libraries = HashSet.create () in
          List.filter_map (unix_lib @ dynlink_lib @ dep_libs @ own_lib)
            ~fn:(fun library_path ->
              if HashSet.insert seen_libraries ~value:library_path then
                Some library_path
              else
                None)
        in
        (* Add cache directories from dependencies to includes *)
        let dep_cache_dirs =
          List.map transitive_deps ~fn:(fun (dep: Dependency.t) -> dep.artifact_dir)
        in
        let binary_includes =
          let unix_includes =
            if needs_unix then
              [ Path.v "+unix" ]
            else
              []
          in
          let dynlink_includes =
            if needs_dynlink then
              [ Path.v "+dynlink" ]
            else
              []
          in
          (* Include current package directory so binaries can access the library *)
          let own_package_dir = [ Path.v "." ] in
          unix_includes @ dynlink_includes @ own_package_dir @ dep_cache_dirs
        in
        List.for_each
          input.package.binaries
          ~fn:(fun (bin: Package.binary) ->
            Module_graph.add_binary_node
              graph_builder
              ~name:bin.name
              ~source:bin.path
              ~libraries:binary_libraries
              ~includes:binary_includes);
        (* Add command nodes for package commands *)
        (* Commands are regular binaries - link all libraries like regular binaries *)
        Log.debug
          ("[MODULE_PLANNER] Command includes for " ^ Package_name.to_string input.package.name ^ ":");
        List.for_each binary_includes ~fn:(fun inc -> Log.debug ("  " ^ Path.to_string inc));
        List.for_each
          input.package.commands
          ~fn:(fun (cmd: Package_command.t) ->
            Module_graph.add_command_node
              graph_builder
              ~name:cmd.name
              ~source:cmd.command_source
              ~libraries:binary_libraries
              ~includes:binary_includes);
        let module_graph = Module_graph.graph graph_builder in
        let analyzed_modules = Module_graph.analyzed_modules graph_builder in
        (
          match Package_layout_validator.validate ~package:input.package ~module_graph ~analyzed_modules with
          | Error _ as err -> err
          | Ok () -> (
              match G.topo_sort module_graph with
              | Error cycle_ids ->
                  let cycle =
                    List.filter_map cycle_ids
                      ~fn:(fun node_id ->
                        match G.get_node module_graph node_id with
                        | None -> None
                        | Some node ->
                            Some (
                              match node.value.kind with
                              | Module_node.ML m
                              | Module_node.MLI m -> Module.module_name m |> Module_name.to_string
                              | Module_node.Library { name; _ } -> "Library(" ^ name ^ ")"
                              | Module_node.Binary { name; _ } -> "Binary(" ^ name ^ ")"
                              | Module_node.Native _ -> "Native"
                              | Module_node.C -> "C"
                              | Module_node.H -> "H"
                              | Module_node.Root -> "Root"
                              | Module_node.Other s -> "Other(" ^ s ^ ")"
                            ))
                    |> List.reverse
                  in
                  Error (Planning_error.CyclicDependency { cycle })
              | Ok sorted_modules -> (
                  let action_graph, _outputs = Action_graph.from_module_graph
                    ~analyzed_modules
                    ~package:input.package
                    ~profile:input.profile
                    ~ctx:input.ctx
                    ~toolchain:input.toolchain
                    ~store:input.store
                    ~depset:input.depset
                    ~needs_unix
                    ~needs_dynlink
                    module_graph in
                  let sources =
                    sorted_modules
                    |> List.map
                      ~fn:(fun (node: Module_node.t G.node) ->
                        match node.value.kind with
                        | Native { files } ->
                            List.map files
                              ~fn:(fun path ->
                                if Path.is_absolute path then
                                  path
                                else
                                  Path.(input.package.path / path))
                        | _ -> (
                            match node.value.file with
                            | Concrete path when Path.to_string path != "" ->
                                let abs_path =
                                  if Path.is_absolute path then
                                    path
                                  else
                                    Path.(input.package.path / path)
                                in
                                [ abs_path ]
                            | _ -> []
                          ))
                    |> List.concat
                  in
                  Ok { sources; module_graph; analyzed_modules; action_graph }
                )
            )
        )
  with
  | exn -> Error (Planning_error.Exception { exn })
