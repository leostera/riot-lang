open Ocaml_platform
open Printf

module Module_name : sig
  type t

  val of_string : string -> t
  val of_path : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let of_string str = String.capitalize_ascii str
  let to_string str = str

  let of_path path =
    let basename = Filename.basename path in
    let name_without_ext =
      try Filename.chop_extension basename with Invalid_argument _ -> basename
    in
    of_string name_without_ext
end

module Namespace : sig
  type t

  val empty : t
  val of_parts : Module_name.t list -> t
  val to_string : t -> string
  val add : t -> Module_name.t -> t
end = struct
  type t = Module_name.t list

  let separator = "__"
  let empty = []
  let of_parts t = t
  let to_string t = String.concat separator (List.map Module_name.to_string t)
  let add t name = t @ [ name ]
end

module Module : sig
  type t

  val of_path : ns:Namespace.t -> string -> t
  val module_name : t -> Module_name.t
  val namespaced_name : t -> string
  val path : t -> string
  val eq : t -> t -> bool
  val kind : t -> [ `implementation | `interface ]
end = struct
  type t = {
    module_name : Module_name.t;
    file_path : string;
    namespaced_name : Namespace.t;
  }

  let eq a b = String.equal a.file_path b.file_path

  let of_path ~ns path =
    let module_name = Module_name.of_path path in
    {
      file_path = path;
      module_name;
      namespaced_name = Namespace.add ns module_name;
    }

  let module_name t = t.module_name
  let namespaced_name t = Namespace.to_string t.namespaced_name
  let path t = t.file_path

  let kind t =
    match Filename.extension t.file_path with
    | ".mli" -> `interface
    | ".ml" -> `implementation
    | _ -> failwith (Format.sprintf "Invalid extension for %S" t.file_path)
end

module Module_registry : sig
  type t

  val create : unit -> t
  val register : t -> Module.t -> Graph.Node_id.t -> unit
  val get : t -> Graph.Node_id.t -> Module.t
  val get_by_name : t -> string -> Graph.Node_id.t
  val print : t -> unit
end = struct
  type t = {
    modules : (Graph.Node_id.t, Module.t) Hashtbl.t;
    intf_by_name : (string, Graph.Node_id.t) Hashtbl.t;
    impl_by_name : (string, Graph.Node_id.t) Hashtbl.t;
  }

  let create () =
    {
      modules = Hashtbl.create 16;
      intf_by_name = Hashtbl.create 16;
      impl_by_name = Hashtbl.create 16;
    }

  let register t mod_ node_id =
    Hashtbl.add t.modules node_id mod_;
    let table =
      match Module.kind mod_ with
      | `implementation -> t.impl_by_name
      | `interface -> t.intf_by_name
    in
    let mod_name = Module.module_name mod_ |> Module_name.to_string in
    Hashtbl.add table mod_name node_id;
    ()

  let get t node_id = Hashtbl.find t.modules node_id
  let get_by_name t name = 
    match Hashtbl.find_opt t.intf_by_name name with
  | Some node -> node
  | None -> Hashtbl.find t.impl_by_name name

  let print t =
    printf "  Registry contains %d modules:\n" (Hashtbl.length t.modules);
    Hashtbl.iter
      (fun node_id mod_ ->
        printf "    Node %s -> %s | %s (%s)\n"
          (Graph.Node_id.to_string node_id)
          (Module.module_name mod_ |> Module_name.to_string)
          (Module.namespaced_name mod_)
          (Module.path mod_))
      t.modules
end

type kind = ML of Module.t | MLI of Module.t | C | H | Other of string | Root

type file =
  | Concrete of string
  | Generated of { path : string; contents : string }

let file_to_string file =
  match file with
  | Concrete str -> str
  | Generated { path; _ } -> path ^ " (generated)"

type dep = {
  file : file;
  mutable open_modules : Graph.Node_id.t list;
  (* Alias modules to open when compiling *)
  kind : kind;
}

type t = {
  root : string;
  src_root : string;
  file_tree : File_scanner.file_tree;
  graph : dep Graph.t;
  registry : Module_registry.t;
  package_name : string;
}

let root_node = { file = Concrete ""; open_modules = []; kind = Root }

let make ~root ~package_name =
  let src_root = Filename.concat root Const.src_dir in
  let file_tree = File_scanner.walk ~root:src_root in
  {
    root;
    src_root;
    file_tree;
    graph = Graph.make ();
    package_name;
    registry = Module_registry.create ();
  }

let to_dot dep_graph =
  Graph.to_dot dep_graph.graph ~name:dep_graph.package_name
    ~node_to_label:(fun dep ->
      match dep.file with
      | Concrete path -> Filename.basename path
      | Generated { path; _ } -> Filename.basename path ^ " (gen)")
    ~node_to_attrs:(fun dep ->
      match dep.kind with
      | MLI _ ->
          [ ("color", "blue"); ("style", "filled"); ("fillcolor", "lightblue") ]
      | ML _ ->
          [
            ("color", "green"); ("style", "filled"); ("fillcolor", "lightgreen");
          ]
      | C ->
          [
            ("color", "red"); ("style", "filled"); ("fillcolor", "lightyellow");
          ]
      | _ -> [])

let iter fn dep_graph =
  let nodes = Graph.topo_sort dep_graph.graph in
  List.iter
    (fun (node : dep Graph.node) ->
      match node.value.kind with Root -> () | _ -> fn node)
    nodes

let print_registry dep_graph = Module_registry.print dep_graph.registry

module Alias_module = struct
  let template (modules : Module.t list) =
    let header = "(* Alias module generated by tusk *)" in
    let body =
      List.map
        (fun mod_ ->
          Format.sprintf "module %s = %s"
            (Module.module_name mod_ |> Module_name.to_string)
            (Module.namespaced_name mod_))
        modules
    in
    String.concat "\n" (header :: body)

  let make_node (ns : Namespace.t) (modules : Module.t list) =
    let mod_ = Module.of_path ~ns "aliases" in
    let path = Module.namespaced_name mod_ ^ ".ml.gen" in
    let file = Generated { path; contents = template modules } in
    let kind = ML mod_ in
    { file; open_modules = []; kind }
end

module Library_interface = struct
  let template (children : Module.t list) =
    let header = "(* Library interface module generated by tusk *)" in
    let body =
      List.map
        (fun mod_ ->
          let name = Module.module_name mod_ |> Module_name.to_string in
          Format.sprintf "module %s = %s" name name)
        children
    in
    String.concat "\n" (header :: body)

  let exist_in_children lib children =
    List.exists
      (fun child ->
        Printf.printf "%s = %s?\n%!" (Module.path lib) (Module.path child);
        Module.eq lib child)
      children

  let make_node (lib : Module.t) (children : Module.t list) aliases_node ~exists
      =
    let path = Module.path lib in
    let file =
      if exists then Concrete path
      else Generated { path; contents = template children }
    in
    let kind =
      match Module.kind lib with
      | `interface -> MLI lib
      | `implementation -> ML lib
    in
    { file; open_modules = [ aliases_node ]; kind }
end

module Ocaml_module = struct
  let make_node (lib : Module.t) aliases =
    let path = Module.path lib in
    let file = Concrete path in
    let kind =
      match Module.kind lib with
      | `interface -> MLI lib
      | `implementation -> ML lib
    in
    let open_modules =
      List.map (fun (alias : dep Graph.node) -> alias.id) aliases
    in
    { file; open_modules; kind }
end

module Dependency_rules = struct
  let is_dependency_allowed ~from_module ~to_module =
    (* Parse "Std__Net__Http__Request" -> ["Std"; "Net"; "Http"; "Request"] *)
    let parse_namespace name =
      String.split_on_char '_' name
      |> List.filter (fun s -> String.length s > 0)
    in

    let from_parts = parse_namespace from_module in
    let to_parts = parse_namespace to_module in

    (* Same module - not a real dependency *)
    if from_parts = to_parts then false
      (* Siblings: same prefix, different last element *)
      (* ["Std"; "Net"; "Http"; "Request"] and ["Std"; "Net"; "Http"; "Response"] *)
    else if
      List.rev from_parts |> List.tl |> List.rev
      = (List.rev to_parts |> List.tl |> List.rev)
    then true
      (* Parent depending on child: to_parts starts with from_parts *)
      (* ["Std"; "Net"] depending on ["Std"; "Net"; "Http"] *)
    else if
      List.length to_parts > List.length from_parts
      && List.take (List.length from_parts) to_parts = from_parts
    then true
    (* Ancestor's sibling: find common ancestor, check if to is its child *)
      else
      (* Find longest common prefix *)
      let rec common_prefix l1 l2 =
        match (l1, l2) with
        | h1 :: t1, h2 :: t2 when h1 = h2 -> h1 :: common_prefix t1 t2
        | _ -> []
      in
      let common = common_prefix from_parts to_parts in

      (* to_module must be exactly one level below common ancestor *)
      (* from ["Std"; "Net"; "Http"; "Client"; "Pool"] can depend on
         ["Std"; "Net"; "Server"] (Server is sibling of Http) *)
      List.length to_parts = List.length common + 1
      && List.length from_parts > List.length common + 1

  (* Examples:
     is_dependency_allowed
       ~from_module:"Std__Net__Http__Request"
       ~to_module:"Std__Net__Http__Response"  (* true - siblings *)

     is_dependency_allowed
       ~from_module:"Std__Net__Http__Request"
       ~to_module:"Std__Net__Tcp"  (* true - Tcp is sibling of Http parent *)

     is_dependency_allowed
       ~from_module:"Std__Net__Http__Request"
       ~to_module:"Std__Net__Http"  (* false - can't depend on parent *)

     is_dependency_allowed
       ~from_module:"Std__Net"
       ~to_module:"Std__Net__Http"  (* true - parent can depend on child *)
  *)
end

type scan_ctx = {
  ns : Namespace.t;
  parent_intf : dep Graph.node;
  parent_impl : dep Graph.node;
  aliases : dep Graph.node list;
}

let rec do_scan ~t ~ctx (file_tree : File_scanner.file_tree) =
  match file_tree with
  | File_scanner.File ({ ext = ".ml" | ".mli"; _ } as file) ->
      handle_ocaml_module ~t ~ctx file
  | File _ -> ()
  | Dir dir -> handle_library ~t ~ctx dir

and handle_ocaml_module ~t ~ctx file =
  let { ns; aliases; parent_impl; parent_intf } = ctx in

  let mod_ = Module.of_path ~ns file.path in
  let node = Ocaml_module.make_node mod_ aliases |> Graph.add_node t.graph in
  printf "Handling OCamml module %S (or %S) at %s\n"
    (Module.module_name mod_ |> Module_name.to_string)
    (Module.namespaced_name mod_)
    file.path;

  Module_registry.register t.registry mod_ node.id;

  let parent =
    match Module.kind mod_ with
    | `interface -> parent_intf
    | `implementation -> parent_impl
  in
  Graph.add_edge parent ~depends_on:node;

  List.iter
    (fun aliases_node -> Graph.add_edge node ~depends_on:aliases_node)
    aliases

and handle_library ~t ~ctx { path; name; children } =
  let { ns; aliases; parent_impl; parent_intf } = ctx in

  let intf_file = t.src_root ^ "/" ^ name ^ ".mli" in
  let intf_mod = Module.of_path ~ns intf_file in

  let impl_file = t.src_root ^ "/" ^ name ^ ".ml" in
  let impl_mod = Module.of_path ~ns impl_file in

  printf "Handling library %S at %s\n" (Module.namespaced_name impl_mod) path;

  let ns = Namespace.add ns (Module.module_name impl_mod) in

  let has_library_interface_ml =
    List.exists
      (fun child ->
        match child with
        | File_scanner.File { path; _ } -> path = impl_file
        | _ -> false)
      children
  in
  let has_library_interface_mli =
    List.exists
      (fun child ->
        match child with
        | File_scanner.File { path; _ } -> path = intf_file
        | _ -> false)
      children
  in

  let children =
    List.filter
      (fun child ->
        match child with
        | File_scanner.File { path; _ } ->
            not (path = intf_file || path = impl_file)
        | _ -> true)
      children
  in

  let child_modules =
    List.map
      (fun child -> Module.of_path ~ns (File_scanner.path child))
      children
  in
  (* First we create the top-level aliases module *)
  let aliases_node =
    let node = Alias_module.make_node ns child_modules in
    Graph.add_node t.graph node
  in

  (* Then we create the library interface for the package *)
  let intf_node =
    let intf =
      Library_interface.make_node intf_mod child_modules aliases_node.id
        ~exists:has_library_interface_mli
    in
    Graph.add_node t.graph intf
  in

  Module_registry.register t.registry intf_mod intf_node.id;

  let impl_node =
    let impl =
      Library_interface.make_node impl_mod child_modules aliases_node.id
        ~exists:has_library_interface_ml
    in
    Graph.add_node t.graph impl
  in

  Module_registry.register t.registry impl_mod impl_node.id;

  (* Add edges between aliases, intf, and impl for the package *)
  Graph.add_edge intf_node ~depends_on:aliases_node;
  Graph.add_edge impl_node ~depends_on:intf_node;

  let ctx =
    {
      ns;
      aliases = aliases @ [ aliases_node ];
      parent_impl = impl_node;
      parent_intf = intf_node;
    }
  in
  List.iter (do_scan ~t ~ctx) children

let scan_from_root t =
  match t.file_tree with
  | File_scanner.Dir dir ->
      let root_node = Graph.add_node t.graph root_node in
      let dir = { dir with name = t.package_name } in

      let ctx =
        {
          ns = Namespace.empty;
          parent_impl = root_node;
          parent_intf = root_node;
          aliases = [];
        }
      in
      handle_library ~t ~ctx dir
  | File file ->
      failwith
        (Format.sprintf "Expected root src dir! Instead found: %S\n" file.path)

let handle_dep t (node : dep Graph.node) =
  let dep = node.value in
  printf "iter %S\n" (file_to_string dep.file);
  match dep.kind with
  | ML mod_ | MLI mod_ ->
      let deps = Ocamldep.get_deps (Module.path mod_) in
      List.iter
        (fun dep ->
          printf "- %s\n" dep;
          let node_id = Module_registry.get_by_name t.registry dep in
          let dep_node = Graph.get_node t.graph node_id in
          Graph.add_edge node ~depends_on:dep_node)
        deps
  | _ -> ()

let wire_deps t = iter (handle_dep t) t

(* This function handles the special case of the root module of the package *)
let scan ~root ~package_name =
  printf "Scanning package %S from %s\n" package_name root;
  let t = make ~root ~package_name in
  scan_from_root t;
  print_registry t;
  wire_deps t;
  t
