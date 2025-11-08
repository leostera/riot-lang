(** Package Planner - Plans individual packages with dependency-aware hashing *)

open Std
open Std.Collections
open Tusk_model
module G = Std.Graph.SimpleGraph

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

(** Compute input hash - fast path that doesn't require ocamldep.

    This hash depends only on:
    - Package metadata (name, deps, binaries, library)
    - Source file contents
    - Dependency hashes

    It does NOT depend on:
    - Module graph (requires ocamldep)
    - Action graph (derived from module graph)

    If input_hash hasn't changed, we know the full hash is the same! *)
let compute_input_hash ~package ~depset ~workspace =
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in

  H.write_string state ("package:" ^ package.Package.name ^ "\n");

  (* Dependencies metadata *)
  let sorted_deps =
    List.sort
      (fun (a : Package.dependency) (b : Package.dependency) ->
        String.compare a.name b.name)
      package.dependencies
  in
  List.iter
    (fun (dep : Package.dependency) ->
      H.write_string state ("dep:" ^ dep.name ^ "\n");
      match dep.source with
      | Package.Workspace -> (
          H.write_string state "dep_source:workspace\n";
          match
            List.find_opt
              (fun (p : Package.t) -> p.name = dep.name)
              workspace.Workspace.packages
          with
          | Some dep_pkg -> (
              H.write_string state
                ("dep_ws_path:" ^ Path.to_string dep_pkg.path ^ "\n");
              match dep_pkg.library with
              | Some _ -> H.write_string state "dep_has_lib:true\n"
              | None -> H.write_string state "dep_has_lib:false\n")
          | None -> ())
      | Package.Path path ->
          H.write_string state
            ("dep_source:path:" ^ Path.to_string path ^ "\n"))
    sorted_deps;

  (* Binaries metadata *)
  let sorted_bins =
    List.sort
      (fun (a : Package.binary) (b : Package.binary) ->
        String.compare a.name b.name)
      package.binaries
  in
  List.iter
    (fun (bin : Package.binary) ->
      H.write_string state ("bin:" ^ bin.name ^ "\n");
      H.write_string state ("bin_path:" ^ Path.to_string bin.path ^ "\n"))
    sorted_bins;

  (* Library metadata *)
  (match package.library with
  | Some lib ->
      H.write_string state "lib:true\n";
      H.write_string state ("lib_path:" ^ Path.to_string lib.path ^ "\n")
  | None -> H.write_string state "lib:false\n");

  (* Source file contents - use files from package.sources, no scanning! *)
  let all_source_files =
    package.sources.src 
    @ package.sources.native 
    @ package.sources.tests
    @ package.sources.examples
  in
  let sorted_files =
    List.sort
      (fun a b -> String.compare (Path.to_string a) (Path.to_string b))
      all_source_files
  in
  List.iter
    (fun file_path ->
      let abs_path =
        if Path.is_absolute file_path then file_path
        else Path.(package.path / file_path)
      in
      let path_str = Path.to_string file_path in
      match Fs.read abs_path with
      | Ok content ->
          H.write_string state ("file:" ^ path_str ^ "\n");
          H.write_string state content;
          H.write_string state "\n"
      | Error _ ->
          (* File read error - include path only *)
          H.write_string state ("file:" ^ path_str ^ "\n"))
    sorted_files;

  (* Dependency hashes *)
  let dep_hashes =
    depset
    |> List.map (fun (dep : Dependency.t) -> dep.hash)
    |> List.sort Std.Crypto.Hash.compare
  in
  List.iter
    (fun hash -> H.write state (Kernel.Crypto.Hash.to_bytes hash))
    dep_hashes;

  H.finish state

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
  if !failed != [] then Error (Failed !failed)
  else if !unplanned != [] then Error (Missing !unplanned)
  else Ok (Vector.to_list depset)

let compute_hash ~package ~sources ~module_graph ~action_graph ~depset
    ~workspace =
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in

  H.write_string state ("package:" ^ package.Package.name ^ "\n");

  let sorted_deps =
    List.sort
      (fun (a : Package.dependency) (b : Package.dependency) ->
        String.compare a.name b.name)
      package.dependencies
  in
  List.iter
    (fun (dep : Package.dependency) ->
      H.write_string state ("dep:" ^ dep.name ^ "\n");
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
                ("dep_ws_path:" ^ Path.to_string dep_pkg.path ^ "\n");
              match dep_pkg.library with
              | Some _ -> H.write_string state "dep_has_lib:true\n"
              | None -> H.write_string state "dep_has_lib:false\n")
          | None -> ())
      | Package.Path path ->
          H.write_string state
            ("dep_source:path:" ^ Path.to_string path ^ "\n"))
    sorted_deps;

  let sorted_bins =
    List.sort
      (fun (a : Package.binary) (b : Package.binary) ->
        String.compare a.name b.name)
      package.binaries
  in
  List.iter
    (fun (bin : Package.binary) ->
      H.write_string state ("bin:" ^ bin.name ^ "\n");
      H.write_string state ("bin_path:" ^ Path.to_string bin.path ^ "\n"))
    sorted_bins;

  (match package.library with
  | Some lib ->
      H.write_string state "lib:true\n";
      H.write_string state ("lib_path:" ^ Path.to_string lib.path ^ "\n")
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
               ("could not read file " ^ Path.to_string file_path ^ " while hashing package " ^
                  path_str package.name)
      in
      H.write_string state ("file:" ^ path_str ^ "\n");
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
  | Ok depset ->
      (* FAST PATH: Compute input hash and check if it exists in store *)
      let input_hash = compute_input_hash ~package ~depset ~workspace in

      if Tusk_store.Store.exists store input_hash then (
        (* Cache hit! Skip expensive planning *)
        Log.info
          "Package %s: fast path (input hash exists in cache, skipping \
           ocamldep)"
          package.name;
        Ok
          (Planned
             {
               package;
               module_graph = G.make ();
               action_graph = Action_graph.create ();
               hash = input_hash;
               depset;
             }))
      else (
        (* Cache miss - do full planning *)
        Log.info "Package %s: slow path (computing full hash with ocamldep)"
          package.name;

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
            (* Use input_hash as the package hash - it's deterministic and sufficient *)
            Ok
              (Planned
                 {
                   package;
                   module_graph;
                   action_graph;
                   hash = input_hash;
                   depset;
                 }))
