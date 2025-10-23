(** Package Planner - Plans individual packages with dependency-aware hashing *)

open Std
open Tusk_model

type plan_result =
  | Planned of {
      package : Package.t;
      module_graph : Module_node.t Graph.SimpleGraph.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
    }
  | MissingDependencies of { package : Package.t; missing : Package.t list }

let check_dependencies_planned ~package_graph ~package =
  let missing =
    Package_graph.get_unplanned_dependencies package_graph package
  in
  if missing = [] then Ok () else Error missing

let compute_hash ~package ~sources ~module_graph ~action_graph
    ~dependency_hashes ~workspace =
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

  let sorted_dep_hashes =
    List.sort Kernel.Crypto.Hash.compare dependency_hashes
  in
  List.iter
    (fun hash -> H.write state (Kernel.Crypto.Hash.to_bytes hash))
    sorted_dep_hashes;

  H.finish state

let plan_package ~workspace ~toolchain ~package_graph ~package =
  match check_dependencies_planned ~package_graph ~package with
  | Error missing -> Ok (MissingDependencies { package; missing })
  | Ok () -> (
      let plan_input =
        Module_planner.
          {
            package;
            toolchain;
            workspace;
            planning_root = Path.v "src";
            dependencies = [];
          }
      in

      match Module_planner.plan_node plan_input with
      | Error err -> Error err
      | Ok { sources; module_graph; action_graph } ->
          let dependency_hashes =
            Package_graph.get_dependency_hashes package_graph package
          in

          let hash =
            compute_hash ~package ~sources ~module_graph ~action_graph
              ~dependency_hashes ~workspace
          in

          Ok (Planned { package; module_graph; action_graph; hash }))
