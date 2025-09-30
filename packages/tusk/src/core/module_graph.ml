(** Module Graph - Intra-package dependency graph for building a single package

    This module builds the intra-package module dependency graph similar to
    minitusk's dep_graph.ml but integrated with tusk's build system. *)

open Std
open Model
open Ocaml

(** Use Std.Graph for graph operations *)
module Graph = Graph.SimpleGraph

(** Graph node representing a module or file to compile *)
type node_kind =
  | MLI of Module_name.t  (** Interface file *)
  | ML of Module_name.t   (** Implementation file *)

type dep = { kind : node_kind; path : Path.t }

(** Module registry to track nodes by name *)
module Registry = struct
  type t = {
    intf_by_name : (string, dep Graph.node) Hashtbl.t;
    impl_by_name : (string, dep Graph.node) Hashtbl.t;
  }

  let create () =
    { intf_by_name = Hashtbl.create 32; impl_by_name = Hashtbl.create 32 }

  let register t node =
    let mod_name =
      match node.Graph.value.kind with
      | MLI name | ML name -> Module_name.to_string name
    in
    (match node.Graph.value.kind with
    | MLI _ -> Hashtbl.add t.intf_by_name mod_name node
    | ML _ -> Hashtbl.add t.impl_by_name mod_name node)

  let find_interface t mod_name = Hashtbl.find_opt t.intf_by_name mod_name
  let find_implementation t mod_name = Hashtbl.find_opt t.impl_by_name mod_name
end

(** The module graph structure *)
type t = {
  package : Workspace.package;
  toolchain : Toolchains.toolchain;
  namespace : Namespace.t;
  graph : dep Graph.t;
  registry : Registry.t;
}

type error = string

(** Create a new module graph for a package *)
let create ~node:(build_node : Build_node.t) ~workspace =
  let package = build_node.package in
  let toolchain = build_node.toolchain in
  let namespace = Namespace.of_list [ String.capitalize_ascii package.name ] in

  {
    package;
    toolchain;
    namespace;
    graph = Graph.make ();
    registry = Registry.create ();
  }

(** Scan package source files and build module nodes *)
let scan_sources t =
  let src_dir = Path.(t.package.path / Path.v "src") in

  (* Find all .ml and .mli files *)
  let sources = ref [] in
  let rec walk_dir dir =
    match Fs.read_dir dir with
    | Error _ -> ()
    | Ok entries ->
        let entry_list = MutIterator.to_list entries in
        List.iter
          (fun entry ->
            let entry_path = Path.(dir / entry) in
            match Fs.is_dir entry_path with
            | Ok true -> walk_dir entry_path
            | Ok false -> (
                match Path.extension entry_path with
                | Some ".ml" | Some ".mli" -> sources := entry_path :: !sources
                | _ -> ())
            | Error _ -> ())
          entry_list
  in
  walk_dir src_dir;
  !sources

(** Register all source files as nodes *)
let register_modules t sources =
  List.iter
    (fun source_path ->
      let is_interface =
        match Path.extension source_path with
        | Some ".mli" -> true
        | _ -> false
      in
      let mod_name = Module_name.of_path source_path in
      let kind = if is_interface then MLI mod_name else ML mod_name in
      let dep = { kind; path = source_path } in
      let node = Graph.add_node t.graph dep in
      Registry.register t.registry node)
    sources

(** Resolve dependencies using ocamldep *)
let resolve_dependencies t =
  (* For each ML/MLI file, run ocamldep to find dependencies *)
  Graph.iter
    (fun _node_id (node : dep Graph.node) ->
      let cwd = Path.to_string t.package.path in
      let file = Path.to_string node.value.path in

      (* Get dependencies using ocamldep *)
      let deps =
        Ocamldep.deps ~toolchain:t.toolchain ~cwd ~file
          ~package_namespace:t.namespace
      in

      (* For each dependency, find the corresponding node and add edge *)
      List.iter
        (fun dep_mod_name ->
          let dep_name = Module_name.to_string dep_mod_name in
          (* First try interface, then implementation *)
          let dep_node_opt =
            match Registry.find_interface t.registry dep_name with
            | Some n -> Some n
            | None -> Registry.find_implementation t.registry dep_name
          in

          match dep_node_opt with
          | Some dep_node ->
              (* Add dependency edge: node depends on dep_node *)
              Graph.add_edge node ~depends_on:dep_node
          | None ->
              (* External dependency (from another package) - ignore *)
              ())
        deps)
    t.graph;

  (* Add edges from .ml to .mli for same module *)
  Hashtbl.iter
    (fun mod_name impl_node ->
      match Registry.find_interface t.registry mod_name with
      | Some intf_node ->
          (* Implementation depends on interface *)
          Graph.add_edge impl_node ~depends_on:intf_node
      | None -> ())
    t.registry.Registry.impl_by_name

(** Generate compilation actions from the module graph *)
let generate_actions t =
  (* Topologically sort the graph to get compilation order *)
  let sorted_nodes =
    try Graph.topo_sort t.graph
    with Graph.Cycle cycle_ids ->
      failwith
        (Printf.sprintf "Cycle detected in module dependencies: %s"
           (String.concat " -> "
              (List.map Graph.Node_id.to_string cycle_ids)))
  in

  (* Generate actions for each node in order *)
  let actions = ref [] in
  List.iter
    (fun (node : dep Graph.node) ->
      match node.value.kind with
      | MLI mod_name ->
          (* Compile interface *)
          let action =
            Actions.CompileInterface {
              source = Path.to_string node.value.path;
              output = Module_name.cmi mod_name;
              includes = ["."];  (* Current directory *)
              flags = [];  (* No special flags for now *)
            }
          in
          actions := action :: !actions
      | ML mod_name ->
          (* Compile implementation *)
          let action =
            Actions.CompileImplementation {
              source = Path.to_string node.value.path;
              output = Module_name.cmo mod_name;
              includes = ["."];
              flags = [];
            }
          in
          actions := action :: !actions)
    sorted_nodes;

  List.rev !actions

(** Build the complete module graph for a package *)
let build ~node ~workspace =
  let t = create ~node ~workspace in

  (* Step 1: Scan source files *)
  let sources = scan_sources t in

  (* Step 2: Register all modules *)
  register_modules t sources;

  (* Step 3: Resolve dependencies with ocamldep *)
  resolve_dependencies t;

  (* Step 4: Generate compilation actions *)
  let actions = generate_actions t in

  Ok (t, actions)
