(** Package Planner - Plans individual packages with dependency-aware hashing *)

open Std
open Std.Collections
open Tusk_model

type plan_result =
  | Planned of {
      package : Package.t;
      module_graph : Module_node.t Graph.SimpleGraph.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
      depset : Dependency.t list;
    }
  | MissingDependencies of { package : Package.t; missing : Package.t list }
  | FailedDependencies of { package : Package.t; failed : Package.t list }

type check_deps_error = Missing of Package.t list | Failed of Package.t list

let check_dependencies_built ~package_graph ~package =
  let deps = Package_graph.get_dependencies package_graph package in

  let depset : Dependency.t vec = vec [] in
  let unplanned = ref [] in
  let failed = ref [] in

  let process_node node =
    let pkg = Package_graph.get_package node in
    match node with
    | Package_graph.Unplanned _ ->
        (* Not yet planned - unplanned dependency *)
        unplanned := pkg :: !unplanned
    | Package_graph.Planned _ ->
        (* Planned but not built yet - treat as unplanned *)
        unplanned := pkg :: !unplanned
    | Package_graph.Failed _ ->
        (* Dependency failed to build *)
        failed := pkg :: !failed
    | Package_graph.Skipped _ ->
        (* Dependency was skipped - treat as failed *)
        failed := pkg :: !failed
    | Package_graph.Built { package; artifact; depset = dep_depset; hash; _ } ->
        let dep = Dependency.{ package; artifact; depset = dep_depset; hash } in
        Vector.push depset dep
  in

  List.iter process_node deps;

  (* Check the sets in order: failed takes precedence *)
  if !failed <> [] then Error (Failed !failed)
  else if !unplanned <> [] then Error (Missing !unplanned)
  else Ok (Vector.to_list depset)

let compute_hash ~package ~sources ~module_graph ~action_graph ~depset
    ~workspace =
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in

  H.write_string state (format "package:%s\n" package.Package.name);

  let sorted_deps =
    List.sort
      (fun (a : Package.dependency) (b : Package.dependency) ->
        String.compare a.name b.name)
      package.dependencies
  in
  List.iter
    (fun (dep : Package.dependency) ->
      H.write_string state (format "dep:%s\n" dep.name);
      match dep.source with
      | Package.Workspace -> (
          H.write_string state "dep_source:workspace\n";
          (* Include info from workspace about this dependency *)
          match
            List.find_opt
              (fun (p : Package.t) -> p.name = dep.name)
              workspace.Workspace.packages
          with
          | Some dep_pkg -> (
              H.write_string state
                (format "dep_ws_path:%s\n" (Path.to_string dep_pkg.path));
              match dep_pkg.library with
              | Some _ -> H.write_string state "dep_has_lib:true\n"
              | None -> H.write_string state "dep_has_lib:false\n")
          | None -> ())
      | Package.Path path ->
          H.write_string state
            (format "dep_source:path:%s\n" (Path.to_string path)))
    sorted_deps;

  let sorted_bins =
    List.sort
      (fun (a : Package.binary) (b : Package.binary) ->
        String.compare a.name b.name)
      package.binaries
  in
  List.iter
    (fun (bin : Package.binary) ->
      H.write_string state (format "bin:%s\n" bin.name);
      H.write_string state (format "bin_path:%s\n" (Path.to_string bin.path)))
    sorted_bins;

  (match package.library with
  | Some lib ->
      H.write_string state "lib:true\n";
      H.write_string state (format "lib_path:%s\n" (Path.to_string lib.path))
  | None -> H.write_string state "lib:false\n");

  let sorted_files =
    List.sort
      (fun a b -> String.compare (Path.to_string a) (Path.to_string b))
      sources
  in
  List.iter
    (fun file_path ->
      let path_str = Path.to_string file_path in
      let content =
        Fs.read file_path
        |> Result.expect
             ~msg:
               (format "could not read file %s while hashing package %s"
                  path_str package.name)
      in
      H.write_string state (format "file:%s\n" path_str);
      H.write_string state content;
      H.write_string state "\n")
    sorted_files;

  let action_nodes = Action_graph.nodes action_graph in
  List.iter
    (fun (node : Action_node.t) ->
      H.write state (Kernel.Crypto.Hash.to_bytes node.value.hash))
    action_nodes;

  (* Hash all dependency hashes from the depset *)
  let dep_hashes =
    depset
    |> List.map (fun (dep : Dependency.t) -> dep.hash)
    |> List.sort Std.Crypto.Hash.compare
  in
  List.iter
    (fun hash -> H.write state (Kernel.Crypto.Hash.to_bytes hash))
    dep_hashes;

  H.finish state

let plan_package ~workspace ~toolchain ~store ~package_graph ~package =
  match check_dependencies_built ~package_graph ~package with
  | Error (Failed failed) -> Ok (FailedDependencies { package; failed })
  | Error (Missing missing) -> Ok (MissingDependencies { package; missing })
  | Ok depset -> (
      (* Get dependencies in correct topological order from package graph *)
      let plan_input =
        Module_planner.
          {
            package;
            toolchain;
            workspace;
            planning_root = Path.v "src";
            depset;
            store;
          }
      in

      match Module_planner.plan_node plan_input with
      | Error err -> Error err
      | Ok { sources; module_graph; action_graph } ->
          let hash =
            compute_hash ~package ~sources ~module_graph ~action_graph ~depset
              ~workspace
          in

          Ok (Planned { package; module_graph; action_graph; hash; depset }))
