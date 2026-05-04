open Stdlib
open Ocaml_platform
open Printf

module Module_name: sig
  type t

  val from_string: string -> t

  val from_path: string -> t

  val to_string: t -> string

  val cma: t -> string
end = struct
  type t = string

  let from_string = fun str ->
    (* Replace hyphens with underscores for valid OCaml module names *)
    let str =
      String.map
        (fun c ->
          if c = '-' then
            '_'
          else
            c)
        str
    in
    (* Uppercase first character only, preserve rest *)
    if String.length str > 0 then
      let first_char = Char.uppercase_ascii str.[0] in
      String.mapi
        (fun i c ->
          if i = 0 then
            first_char
          else
            c)
        str
    else
      str

  let to_string = fun str -> str

  let from_path = fun path ->
    let basename = Filename.basename path in
    let name_without_ext =
      try Filename.chop_extension basename with
      | Invalid_argument _ -> basename
    in
    from_string name_without_ext

  let cma = fun t -> t ^ ".cma"
end

module Namespace: sig
  type t

  val empty: t

  val from_parts: Module_name.t list -> t

  val to_string: t -> string

  val add: t -> Module_name.t -> t

  val is_empty: t -> bool
end = struct
  type t = Module_name.t list

  let separator = "__"

  let empty = []

  let from_parts = fun t -> t

  let to_string = fun t -> String.concat separator (List.map Module_name.to_string t)

  let add = fun t name -> t @ [ name ]

  let is_empty = fun t -> t = []
end

module Module: sig
  type t

  val from_path: ns:Namespace.t -> string -> t

  val module_name: t -> Module_name.t

  val namespaced_name: t -> string

  val path: t -> string

  val cmi: t -> string

  val cmo: t -> string

  val eq: t -> t -> bool

  val kind: t -> [`implementation | `interface]

  val is_aliases: t -> bool
end = struct
  type t = {
    module_name: Module_name.t;
    file_path: string;
    namespaced_name: Namespace.t;
  }

  let eq = fun a b -> String.equal a.file_path b.file_path

  let from_path = fun ~ns path ->
    let module_name = Module_name.from_path path in
    { file_path = path; module_name; namespaced_name = Namespace.add ns module_name }

  let module_name = fun t -> t.module_name

  let namespaced_name = fun t -> Namespace.to_string t.namespaced_name

  let path = fun t -> t.file_path

  let kind = fun t ->
    match Filename.extension t.file_path with
    | ".mli" -> `interface
    | ".ml" -> `implementation
    | _ -> failwith (Format.sprintf "Invalid extension for %S" t.file_path)

  let cmi = fun t -> namespaced_name t ^ ".cmi"

  let cmo = fun t -> namespaced_name t ^ ".cmo"

  let is_aliases = fun t -> Module_name.to_string t.module_name = "Aliases"
end

module Build_results: sig
  type t

  val create: unit -> t

  (* Register a package with it's module interface *)

  (* Register a package with it's module interface *)
  val register:
    t ->
    Package.t ->
    Module_name.t ->
    outputs:string list ->
    cc_flags:string list ->
    ld_flags:string list ->
    unit

  val has_module: t -> Module_name.t -> bool

  val copy_to_sandbox: t -> string -> unit

  val get_package_names: t -> string list

  val get_transitive_cc_flags: t -> string list -> string list

  val get_transitive_ld_flags: t -> string list -> string list

  val has_stdlib: t -> string list -> bool

  val has_unix: t -> string list -> bool

  val has_dynlink: t -> string list -> bool
end = struct
  (* Build results tracking for cross-package dependencies *)

  (*
     in this module we want to

     1. save a package's interface name (kernel for packages/kernel)
     2. associate its outputs with it

     so that later on when we're creating actions from a dep_graph
     we can copy the outputs associated with this package

     and so when adding edges to things we'll basically check if something is an external dep (is in build_results)
     and if it isn't then complain

     i mean we don't need the thing to be magic or super flexible, this is just for bootstrapping riot
  *)

  type entry = {
    package: Package.t;
    module_name: Module_name.t;
    outputs: string list;
    cc_flags: string list;
    ld_flags: string list;
    uses_stdlib: bool;
    uses_unix: bool;
    uses_dynlink: bool;
  }

  type t = {
    (* Map from package name (e.g., "kernel") to list of output paths *)
    packages: (Module_name.t, entry) Hashtbl.t;
    (* Track order of registration *)
    mutable order: Module_name.t list;
  }

  let create = fun () -> { packages = Hashtbl.create 8; order = [] }

  let register = fun t package module_name ~outputs ~cc_flags ~ld_flags ->
    (* Add to order list if not already there *)
    if not (Hashtbl.mem t.packages module_name) then
      t.order <- t.order @ [ module_name ];
    Hashtbl.replace
      t.packages
      module_name
      {
        package;
        module_name;
        outputs;
        cc_flags;
        ld_flags;
        uses_stdlib = Package.uses_stdlib package;
        uses_unix = Package.uses_unix package;
        uses_dynlink = Package.uses_dynlink package;
      }

  let has_module = fun t module_name -> Hashtbl.mem t.packages module_name

  let copy_to_sandbox = fun t sandbox_dir ->
    (* Copy all package outputs to the sandbox *)
    Hashtbl.iter
      (fun _mod_name entry ->
        List.iter
          (fun src ->
            if Io.file_exists src then (
              let dst = Filename.concat sandbox_dir (Filename.basename src) in
              Io.copy_file src dst;
              printf "  Copied dependency: %s\n" (Filename.basename src)
            ))
          entry.outputs)
      t.packages

  let get_package_names = fun t ->
    (* Get list of all registered package names in registration order *)
    List.map Module_name.to_string t.order

  let get_transitive_cc_flags = fun t package_names ->
    (* Collect cc_flags from all dependency packages *)
    let all_flags = ref [] in
    List.iter
      (fun pkg_name ->
        let mod_name = Module_name.from_string pkg_name in
        match Hashtbl.find_opt t.packages mod_name with
        | Some entry -> all_flags := !all_flags @ entry.cc_flags
        | None -> ())
      package_names;
    !all_flags

  let get_transitive_ld_flags = fun t package_names ->
    (* Collect ld_flags from all dependency packages *)
    let all_flags = ref [] in
    List.iter
      (fun pkg_name ->
        let mod_name = Module_name.from_string pkg_name in
        match Hashtbl.find_opt t.packages mod_name with
        | Some entry -> all_flags := !all_flags @ entry.ld_flags
        | None -> ())
      package_names;
    !all_flags

  let has_stdlib = fun t package_names ->
    (* Check if any dependency package uses stdlib *)
    List.exists
      (fun pkg_name ->
        let mod_name = Module_name.from_string pkg_name in
        match Hashtbl.find_opt t.packages mod_name with
        | Some entry -> entry.uses_stdlib
        | None -> false)
      package_names

  let has_unix = fun t package_names ->
    (* Check if any dependency package uses unix *)
    List.exists
      (fun pkg_name ->
        let mod_name = Module_name.from_string pkg_name in
        match Hashtbl.find_opt t.packages mod_name with
        | Some entry -> entry.uses_unix
        | None -> false)
      package_names

  let has_dynlink = fun t package_names ->
    (* Check if any dependency package uses dynlink *)
    List.exists
      (fun pkg_name ->
        let mod_name = Module_name.from_string pkg_name in
        match Hashtbl.find_opt t.packages mod_name with
        | Some entry -> entry.uses_dynlink
        | None -> false)
      package_names

  let print = fun t ->
    printf "\n=== Build Results ===\n";
    Hashtbl.iter
      (fun mod_name entry ->
        printf
          "Package %s: %d outputs\n"
          (Module_name.to_string mod_name)
          (List.length entry.outputs);
        List.iter (fun out -> printf "  - %s\n" (Filename.basename out)) entry.outputs)
      t.packages
end

module Module_registry: sig
  type t

  val create: unit -> t

  val register: t -> Module.t -> Graph.Node_id.t -> unit

  val get: t -> Graph.Node_id.t -> Module.t

  val get_by_name: t -> Module_name.t -> Graph.Node_id.t list

  (* Returns list of nodes *)
  val print: t -> unit
end = struct
  type t = {
    modules: (Graph.Node_id.t, Module.t) Hashtbl.t;
    intf_by_name: (Module_name.t, Graph.Node_id.t) Hashtbl.t;
    impl_by_name: (Module_name.t, Graph.Node_id.t) Hashtbl.t;
  }

  let create = fun () -> {
    modules = Hashtbl.create 16;
    intf_by_name = Hashtbl.create 16;
    impl_by_name = Hashtbl.create 16;
  }

  let register = fun t mod_ node_id ->
    Hashtbl.add t.modules node_id mod_;
    let table =
      match Module.kind mod_ with
      | `implementation -> t.impl_by_name
      | `interface -> t.intf_by_name
    in
    let mod_name = Module.module_name mod_ in
    Hashtbl.add table mod_name node_id;
    ()

  let get = fun t node_id -> Hashtbl.find t.modules node_id

  let get_by_name = fun t name ->
    (* Use find_all to get ALL bindings for this name *)
    (* This is important because multiple modules can have the same simple name *)
    (* (e.g., Std__Fs__Event and Std__Log__Event both have simple name "Event") *)
    let intf_nodes = Hashtbl.find_all t.intf_by_name name in
    let impl_nodes = Hashtbl.find_all t.impl_by_name name in
    let all_nodes = intf_nodes @ impl_nodes in
    match all_nodes with
    | [] -> raise Not_found
    | nodes -> nodes

  let print = fun t ->
    printf "  Registry contains %d modules:\n" (Hashtbl.length t.modules);
    (* Collect all entries and sort them *)
    let entries = Hashtbl.fold (fun node_id mod_ acc -> (node_id, mod_) :: acc) t.modules [] in
    let sorted_entries =
      List.sort
        (fun (_, mod1) (_, mod2) ->
          String.compare
            (
              Module.module_name mod1
              |> Module_name.to_string
            )
            (
              Module.module_name mod2
              |> Module_name.to_string
            ))
        entries
    in
    List.iter
      (fun (node_id, mod_) ->
        printf
          "    Node %s -> %s | %s (%s)\n"
          (Graph.Node_id.to_string node_id)
          (
            Module.module_name mod_
            |> Module_name.to_string
          )
          (Module.namespaced_name mod_)
          (Module.path mod_))
      sorted_entries
end

type kind =
  | ML of Module.t
  | MLI of Module.t
  | C
  | H
  | Other of string
  | Root

type file =
  | Concrete of string
  | Generated of { path: string; contents: string }

let file_to_string = fun file ->
  match file with
  | Concrete str -> str
  | Generated { path; _ } -> path ^ " (generated)"

type dep = {
  file: file;
  mutable open_modules: dep Graph.node list;
  (* Alias modules to open when compiling *)
  kind: kind;
}

type t = {
  root: string;
  src_root: string;
  file_tree: File_scanner.file_tree;
  graph: dep Graph.t;
  registry: Module_registry.t;
  build_results: Build_results.t;
  package_name: Module_name.t;
  package: Package.t;
}

let root_node = { file = Concrete ""; open_modules = []; kind = Root }

let make = fun ~root ~package ~build_results ->
  let src_root = Filename.concat root Const.src_dir in
  let file_tree = File_scanner.walk ~root:src_root in
  {
    root;
    src_root;
    file_tree;
    graph = Graph.make ();
    package_name = Module_name.from_string package.Package.name;
    registry = Module_registry.create ();
    build_results;
    package;
  }

let to_dot = fun dep_graph ->
  Graph.to_dot
    dep_graph.graph
    ~name:(Module_name.to_string dep_graph.package_name)
    ~node_to_label:(fun dep ->
      match dep.file with
      | Concrete path -> Filename.basename path
      | Generated { path; _ } -> Filename.basename path ^ " (gen)")
    ~node_to_attrs:(fun dep ->
      match dep.kind with
      | MLI _ -> [ ("color", "blue"); ("style", "filled"); ("fillcolor", "lightblue"); ]
      | ML _ -> [ ("color", "green"); ("style", "filled"); ("fillcolor", "lightgreen"); ]
      | C -> [ ("color", "red"); ("style", "filled"); ("fillcolor", "lightyellow"); ]
      | _ -> [])

let iter = fun fn dep_graph ->
  try
    let nodes = Graph.topo_sort dep_graph.graph in
    List.iter
      (fun (node: dep Graph.node) ->
        match node.value.kind with
        | Root -> ()
        | _ -> fn node)
      nodes
  with
  | Graph.Cycle node_ids ->
      Printf.printf "\n!!! ERROR: Dependency cycle detected !!!\n";
      Printf.printf "The following modules form a cycle:\n\n";
      List.iter
        (fun node_id ->
          try
            let node = Graph.get_node dep_graph.graph node_id in
            let module_info =
              match node.value.file with
              | Concrete path -> Printf.sprintf "%s" path
              | Generated { path; _ } -> Printf.sprintf "%s (generated)" path
            in
            Printf.printf "  Node %d: %s\n" (Graph.Node_id.to_int node_id) module_info
          with
          | Not_found -> Printf.printf "  Node %d: <unknown>\n" (Graph.Node_id.to_int node_id))
        node_ids;
      Printf.printf "\nPlease check the dependency graph for circular dependencies.\n";
      raise (Graph.Cycle node_ids)

let print_registry = fun dep_graph -> Module_registry.print dep_graph.registry

module Alias_module = struct
  let template = fun (modules: Module.t list) ->
    let header = "(* Alias module generated by riot *)" in
    (* Deduplicate by module name - only implementations, one alias per module *)
    let unique_modules =
      modules
      |> List.filter_map
        (fun mod_ ->
          match Module.kind mod_ with
          | `interface -> None
          | `implementation ->
              Some (Module.module_name mod_
              |> Module_name.to_string, Module.namespaced_name mod_))
      |> List.sort_uniq (fun (n1, _) (n2, _) -> String.compare n1 n2)
    in
    let body =
      List.map (fun (name, ns) -> Format.sprintf "module %s = %s" name ns) unique_modules
    in
    let super_body =
      if unique_modules = [] then
        []
      else
        ([ ""; "module Super = struct" ] @ body) @ [ "end" ]
    in
    String.concat "\n" ((header :: body) @ super_body)

  let make_node = fun (ns: Namespace.t) (modules: Module.t list) ->
    let mod_ = Module.from_path ~ns "aliases" in
    let path = Module.namespaced_name mod_ ^ ".ml.gen" in
    let file = Generated { path; contents = template modules } in
    let kind = ML mod_ in
    { file; open_modules = []; kind }
end

module Library_interface = struct
  let template = fun (children: Module.t list) ->
    let header = "(* Library interface module generated by riot *)" in
    (* Deduplicate by module name - we only need one entry per module *)
    let unique_names =
      children
      |> List.map
        (fun mod_ ->
          Module.module_name mod_
          |> Module_name.to_string)
      |> List.sort_uniq String.compare
    in
    let body = List.map (fun name -> Format.sprintf "module %s = %s" name name) unique_names in
    String.concat "\n" (header :: body)

  let exist_in_children = fun lib children ->
    List.exists
      (fun child ->
        printf "%s = %s?\n%!" (Module.path lib) (Module.path child);
        Module.eq lib child)
      children

  let make_node = fun (lib: Module.t) (children: Module.t list) aliases ~exists ~actual_path ->
    let path =
      match actual_path with
      | Some p -> p
      | None -> Module.path lib
    in
    let file =
      if exists then
        Concrete path
      else
        Generated { path; contents = template children }
    in
    let kind =
      match Module.kind lib with
      | `interface -> MLI lib
      | `implementation -> ML lib
    in
    let open_modules = aliases in
    { file; open_modules; kind }
end

module Ocaml_module = struct
  let make_node = fun (lib: Module.t) aliases ->
    let path = Module.path lib in
    let file = Concrete path in
    let kind =
      match Module.kind lib with
      | `interface -> MLI lib
      | `implementation -> ML lib
    in
    let open_modules = aliases in
    { file; open_modules; kind }
end

module Dependency_rules = struct
  let is_dependency_allowed = fun ~from_module ~to_module ->
    (* Parse "Std__Net__Http__Request" -> ["Std"; "Net"; "Http"; "Request"] *)
    let parse_namespace name =
      String.split_on_char '_' name
      |> List.filter (fun s -> String.length s > 0)
    in
    let from_parts = parse_namespace from_module in
    let to_parts = parse_namespace to_module in
    (* Same module - not a real dependency *)
    if from_parts = to_parts then
      false
      (* Siblings: same prefix, different last element *)
      (* ["Std"; "Net"; "Http"; "Request"] and ["Std"; "Net"; "Http"; "Response"] *)
    else if (
      List.rev from_parts
      |> List.tl
      |> List.rev
    ) = (
      List.rev to_parts
      |> List.tl
      |> List.rev
    ) then
      true
    (* Parent depending on child: to_parts starts with from_parts *)
    (* ["Std"; "Net"] depending on ["Std"; "Net"; "Http"] *)
    else if
      List.length to_parts > List.length from_parts
      && List.take (List.length from_parts) to_parts = from_parts
    then
      true
    (* Ancestor's sibling: find common ancestor, check if to is its child *)
    else
      (* Find longest common prefix *)
      let rec common_prefix l1 l2 =
        match (l1, l2) with
        | (h1 :: t1, h2 :: t2) when h1 = h2 -> h1 :: common_prefix t1 t2
        | _ -> []
      in
      let common = common_prefix from_parts to_parts in
      (* to_module must be exactly one level below common ancestor *)
      (* from ["Std"; "Net"; "Http"; "Client"; "Pool"] can depend on
         ["Std"; "Net"; "Server"] (Server is sibling of Http)
      *)
      List.length to_parts = List.length common + 1
      && List.length from_parts > List.length common + 1
end

type scan_ctx = {
  ns: Namespace.t;
  parent_intf: dep Graph.node;
  parent_impl: dep Graph.node;
  aliases: dep Graph.node list;
}

let rec do_scan = fun ~t ~ctx (file_tree: File_scanner.file_tree) ->
  match file_tree with
  | File_scanner.File ({ ext = ".ml"
  | ".mli"; _ } as file) ->
      handle_ocaml_module ~t ~ctx file
  | File_scanner.File ({ ext = ".c"; _ } as file) -> handle_c_file ~t ~ctx file
  | File_scanner.File ({ ext = ".h"; _ } as file) -> handle_h_file ~t ~ctx file
  | File_scanner.File file -> printf "Skipping file with ext=%s: %s\n" file.ext file.path
  | Dir dir -> handle_library ~t ~ctx dir

and handle_c_file = fun ~t ~ctx file ->
  (* printf "HANDLE_C_FILE: %s\n" file.path; *)
  let { parent_impl; _ } = ctx in
  (* Create a C file node *)
  let node = { file = Concrete file.path; open_modules = []; kind = C } in
  let c_node = Graph.add_node t.graph node in
  (* printf "  Added C node %d for %s\n" (Graph.Node_id.to_int c_node.id) file.path; *)
  (* C files are dependencies of the implementation *)
  Graph.add_edge parent_impl ~depends_on:c_node

and handle_h_file = fun ~t ~ctx file ->
  (* Header files just need to be copied, not compiled *)
  let node = { file = Concrete file.path; open_modules = []; kind = H } in
  let _h_node = Graph.add_node t.graph node in
  ()

and handle_ocaml_module = fun ~t ~ctx file ->
  let {
    ns;
    aliases;
    parent_impl;
    parent_intf;
  } = ctx
  in
  let mod_ = Module.from_path ~ns file.path in
  (* Debug output for event files *)
  let basename = Filename.basename file.path in
  if String.contains basename 'e' && String.contains basename 'v' then
    Printf.printf
      "[DEBUG] Processing module: %s -> %s (ns=%s)\n"
      file.path
      (Module.namespaced_name mod_)
      (Namespace.to_string ns);
  let is_binary =
    List.exists
      (fun (bin: Package.binary) ->
        let bin_basename = Filename.basename bin.path in
        let file_basename = Filename.basename file.path in
        bin_basename = file_basename)
      (Package.binaries t.package)
  in
  if is_binary then (
    Printf.printf "[DEBUG] Skipping binary module: %s\n" file.path;
    ()
  ) else
    let node =
      Ocaml_module.make_node mod_ aliases
      |> Graph.add_node t.graph
    in
    (* printf "Handling OCamml module %S (or %S) at %s\n"
       (Module.module_name mod_ |> Module_name.to_string)
       (Module.namespaced_name mod_)
       file.path;
    *)
    (* Debug Event registration *)
    if Module_name.to_string (Module.module_name mod_) = "Event" then
      Printf.printf
        "[DEBUG] Registering Event: %s (namespaced=%s)\n"
        file.path
        (Module.namespaced_name mod_);
  Module_registry.register t.registry mod_ node.id;
  (
    match Module.kind mod_ with
    | `implementation -> (
        (* Try to find the interface node in the registry *)
        let mod_name = Module.module_name mod_ in
        try
          let node_ids = Module_registry.get_by_name t.registry mod_name in
          List.iter
            (fun intf_node_id ->
              let intf_node = Graph.get_node t.graph intf_node_id in
              (* Check if it's actually an interface *)
              match intf_node.value.kind with
              | MLI intf_mod when Module.module_name intf_mod = mod_name ->
                  (* Add edge from implementation to interface *)
                  Graph.add_edge node ~depends_on:intf_node
              | _ -> ())
            node_ids
        with
        | Not_found -> ()
      )
    | `interface -> ()
  );
  let parent =
    match Module.kind mod_ with
    | `interface -> parent_intf
    | `implementation -> parent_impl
  in
  Graph.add_edge parent ~depends_on:node;
  List.iter (fun aliases_node -> Graph.add_edge node ~depends_on:aliases_node) aliases

and handle_library = fun ~t ~ctx { path; name; children } ->
  let {
    ns;
    aliases;
    parent_impl;
    parent_intf;
  } = ctx
  in
  (* For the root library (when ns is empty), use package name; otherwise use the dir path *)
  let base_path =
    if Namespace.is_empty ns then
      t.src_root ^ "/" ^ name
    else
      (* Nested library: packages/std/src/data/data *)
      path ^ "/" ^ name
  in
  let intf_file = base_path ^ ".mli" in
  let intf_mod = Module.from_path ~ns intf_file in
  let impl_file = base_path ^ ".ml" in
  let impl_mod = Module.from_path ~ns impl_file in
  (*
     printf "Handling library %S at %s\n" (Module.namespaced_name impl_mod) path;
  *)
  let ns = Namespace.add ns (Module.module_name impl_mod) in
  (* Only create modules for ML/MLI files, not directories *)
  let children =
    List.map
      (fun child ->
        match child with
        | File_scanner.File { ext = ".ml"
          | ".mli"; _ } ->
            (Some (Module.from_path ~ns (File_scanner.path child)), child)
        | _ -> (None, child))
      children
  in
  (* Check if library interface files exist among children by comparing module names *)
  (* Also get the actual file path if it exists *)
  let library_interface_ml_path =
    List.find_map
      (fun (mod_opt, child) ->
        match (mod_opt, child) with
        | (Some mod_, File_scanner.File { path; _ }) ->
            (* Compare module names, not paths - this handles case normalization *)
            if
              Module_name.to_string (Module.module_name mod_)
              = Module_name.to_string (Module.module_name impl_mod)
              && Filename.extension path = ".ml"
            then
              Some path
            else
              None
        | _ -> None)
      children
  in
  let library_interface_mli_path =
    List.find_map
      (fun (mod_opt, child) ->
        match (mod_opt, child) with
        | (Some mod_, File_scanner.File { path; _ }) ->
            (* Compare module names, not paths - this handles case normalization *)
            if
              Module_name.to_string (Module.module_name mod_)
              = Module_name.to_string (Module.module_name intf_mod)
              && Filename.extension path = ".mli"
            then
              Some path
            else
              None
        | _ -> None)
      children
  in
  let has_library_interface_ml = library_interface_ml_path <> None in
  let has_library_interface_mli = library_interface_mli_path <> None in
  let children_without_lib_files =
    (* We need to filter out library interface files that will serve as library interfaces *)
    (* for their subdirectories, like collections/collections.ml -> Kernel__Collections *)
    (* These files should NOT be included as regular modules in the parent *)
    (* First, get all directory names so we can detect dir/dir.ml patterns *)
    let dir_names =
      List.filter_map
        (fun (_, child) ->
          match child with
          | File_scanner.Dir { name; _ } -> Some name
          | _ -> None)
        children
      |> List.map String.lowercase_ascii
      |> List.sort_uniq String.compare
    in
    List.filter
      (fun (mod_opt, child) ->
        match (mod_opt, child) with
        | (Some mod_, File_scanner.File { path; _ }) ->
            (* Check if this is the root library interface (kernel.ml/kernel.mli) *)
            let is_lib_ml =
              match library_interface_ml_path with
              | Some p -> path = p
              | None -> false
            in
            let is_lib_mli =
              match library_interface_mli_path with
              | Some p -> path = p
              | None -> false
            in
            (* Check if this is a subdirectory library interface (collections/collections.ml) *)
            let basename =
              Filename.basename path
              |> Filename.remove_extension
              |> String.lowercase_ascii
            in
            let is_subdir_lib = List.mem basename dir_names in
            (* Filter out both root and subdirectory library interface files *)
            not (is_lib_ml || is_lib_mli || is_subdir_lib)
        | _ -> true)
      children
  in
  (* Get modules from files first *)
  let file_modules =
    List.filter_map
      (fun (mod_opt, child) ->
        match (mod_opt, child) with
        | (Some mod_, File_scanner.File { ext = ".mli"
                      | ".ml"; _ }) -> Some mod_
        | _ -> None)
      children_without_lib_files
  in
  (* Get module names that already exist from files *)
  let existing_module_names =
    List.map
      (fun m ->
        Module.module_name m
        |> Module_name.to_string)
      file_modules
    |> List.sort_uniq String.compare
  in
  (* Add directories only if there's no file module with the same name *)
  let dir_modules =
    List.filter_map
      (fun (mod_opt, child) ->
        match (mod_opt, child) with
        | (None, File_scanner.Dir { name; path; _ }) ->
            let module_name = Module_name.from_string name in
            if List.mem (Module_name.to_string module_name) existing_module_names then
              None
              (* Skip directories that have corresponding .ml/.mli files *)
            else
              (* For directories, create a module from the directory name with .ml extension *)
              (* This is just for the alias generation - the directory itself doesn't become a module *)
              Some (Module.from_path ~ns (path ^ "/" ^ name ^ ".ml"))
        | _ -> None)
      children_without_lib_files
  in
  (* Filter out binary modules from child_modules so they don't get included in the library interface *)
  Printf.printf "[DEBUG] File modules:\n";
  List.iter (fun m -> Printf.printf "  - %s\n" (Module.path m)) file_modules;
  let is_binary_module mod_ =
    let mod_path = Module.path mod_ in
    List.exists
      (fun (bin: Package.binary) ->
        let bin_basename = Filename.basename bin.path in
        let mod_basename = Filename.basename mod_path in
        let matches = mod_path = bin.path || mod_basename = bin_basename in
        if matches then
          Printf.printf
            "[DEBUG] MATCHED: %s == %s (basename: %s == %s)\n"
            mod_path
            bin.path
            mod_basename
            bin_basename;
        matches)
      (Package.binaries t.package)
  in
  let library_modules =
    List.filter (fun m -> not (is_binary_module m)) (file_modules @ dir_modules)
  in
  Printf.printf
    "[DEBUG] After filtering: %d modules (was %d)\n"
    (List.length library_modules)
    (List.length (file_modules @ dir_modules));
  let child_modules = library_modules in
  (* First we create the top-level aliases module *)
  let aliases_node =
    let node = Alias_module.make_node ns child_modules in
    Graph.add_node t.graph node
  in
  (* If a library has a root .ml but no root .mli, do not synthesize a fake
     interface node. The implementation-produced .cmi is the public surface.
  *)
  let (intf_node, impl_node) =
    if has_library_interface_ml && not has_library_interface_mli then
      let impl =
        Library_interface.make_node
          impl_mod
          child_modules
          (aliases @ [ aliases_node ])
          ~exists:true
          ~actual_path:library_interface_ml_path
      in
      let impl_node = Graph.add_node t.graph impl in
      Module_registry.register t.registry intf_mod impl_node.id;
      Module_registry.register t.registry impl_mod impl_node.id;
      Graph.add_edge impl_node ~depends_on:aliases_node;
      (impl_node, impl_node)
    else
      (
        (* Then we create the library interface for the package *)
        let intf_node =
          let intf =
            Library_interface.make_node
              intf_mod
              child_modules
              (aliases @ [ aliases_node ])
              ~exists:has_library_interface_mli
              ~actual_path:library_interface_mli_path
          in
          Graph.add_node t.graph intf
        in
        Module_registry.register t.registry intf_mod intf_node.id;
        let impl_node =
          let impl =
            Library_interface.make_node
              impl_mod
              child_modules
              (aliases @ [ aliases_node ])
              ~exists:has_library_interface_ml
              ~actual_path:library_interface_ml_path
          in
          Graph.add_node t.graph impl
        in
        Module_registry.register t.registry impl_mod impl_node.id;
        (* Add edges between aliases, intf, and impl for the package *)
        Graph.add_edge intf_node ~depends_on:aliases_node;
        Graph.add_edge impl_node ~depends_on:aliases_node;
        Graph.add_edge impl_node ~depends_on:intf_node;
        (intf_node, impl_node)
      )
  in
  let ctx = {
    ns;
    aliases = aliases @ [ aliases_node ];
    parent_impl = impl_node;
    parent_intf = intf_node;
  }
  in
  (* Sort children to process directories FIRST, then .mli files, then .ml files *)
  (* This ensures subdirectory library interfaces are created before files that depend on them *)
  let sorted_children =
    List.sort
      (fun (_, a) (_, b) ->
        let priority child =
          match child with
          | File_scanner.Dir _ -> 0
          | File_scanner.File { ext = ".mli"; _ } -> 1
          | File_scanner.File { ext = ".ml"; _ } -> 2
          | _ -> 3
        in
        Int.compare (priority a) (priority b))
      children_without_lib_files
  in
  (* Process subdirectories first to create their library interfaces *)
  (* Collect the subdirectory library interface nodes so we can add dependencies *)
  let subdir_lib_intfs = ref [] in
  List.iter
    (fun (_mod_opt, child) ->
      match child with
      | File_scanner.Dir { name; _ } ->
          (* After processing this directory, look up its library interface node *)
          do_scan ~t ~ctx child;
          let subdir_mod_name = Module_name.from_string name in
          let subdir_namespaced_name =
            Namespace.add ns subdir_mod_name
            |> Namespace.to_string
          in
          (* Prefer the sub-library interface when it exists, but fall back to the
             implementation node when the library exposes its surface via .ml only.
          *)
          let node_ids = Module_registry.get_by_name t.registry subdir_mod_name in
          let find_subdir_surface expected_kind =
            List.find_map
              (fun node_id ->
                try
                  let node = Graph.get_node t.graph node_id in
                  match (expected_kind, node.value.kind) with
                  | (`interface, MLI mod_)
                  | (`implementation, ML mod_) ->
                      if Module.namespaced_name mod_ = subdir_namespaced_name then
                        Some node
                      else
                        None
                  | _ -> None
                with
                | Not_found -> None)
              node_ids
          in
          (
            match find_subdir_surface `interface with
            | Some node -> subdir_lib_intfs := node :: !subdir_lib_intfs
            | None -> (
                match find_subdir_surface `implementation with
                | Some node -> subdir_lib_intfs := node :: !subdir_lib_intfs
                | None -> ()
              )
          )
      | _ -> ())
    sorted_children;
  (* Add dependencies from parent library interface to subdirectory library interfaces *)
  (* This ensures subdirectory library interfaces are compiled before the parent *)
  Printf.printf
    "[DEBUG] Adding %d subdir lib dependencies to %s\n"
    (List.length !subdir_lib_intfs)
    (Module.namespaced_name intf_mod);
  List.iter
    (fun (subdir_intf_node: dep Graph.node) ->
      let subdir_name =
        match subdir_intf_node.value.kind with
        | MLI m
        | ML m -> Module.namespaced_name m
        | _ -> "?"
      in
      Printf.printf "[DEBUG]   %s depends on %s\n" (Module.namespaced_name intf_mod) subdir_name;
      Graph.add_edge intf_node ~depends_on:subdir_intf_node;
      Graph.add_edge impl_node ~depends_on:subdir_intf_node)
    !subdir_lib_intfs;
  (* Now process all non-directory children *)
  List.iter
    (fun (_mod_opt, child) ->
      match child with
      | File_scanner.Dir _ -> ()
      | _ -> do_scan ~t ~ctx child)
    sorted_children

let scan_from_root = fun t ->
  match t.file_tree with
  | File_scanner.Dir dir ->
      let root_node = Graph.add_node t.graph root_node in
      let dir = { dir with name = Module_name.to_string t.package_name } in
      let ctx = {
        ns = Namespace.empty;
        parent_impl = root_node;
        parent_intf = root_node;
        aliases = [];
      }
      in
      handle_library ~t ~ctx dir
  | File file -> failwith (Format.sprintf "Expected root src dir! Instead found: %S\n" file.path)

let has_matching_interface_surface = fun t (node: dep Graph.node) ->
  match node.value.kind with
  | ML mod_ ->
      let namespaced_name = Module.namespaced_name mod_ in
      let mod_name = Module.module_name mod_ in
      (
        try
          let node_ids = Module_registry.get_by_name t.registry mod_name in
          List.exists
            (fun node_id ->
              let candidate = Graph.get_node t.graph node_id in
              match candidate.value.kind with
              | MLI candidate_mod -> Module.namespaced_name candidate_mod = namespaced_name
              | _ -> false)
            node_ids
        with
        | Not_found -> false
      )
  | _ -> false

let handle_dep = fun t (node: dep Graph.node) ->
  let dep = node.value in
  (* printf "iter %S\n" (file_to_string dep.file); *)
  match dep.kind with
  | ML mod_
  | MLI mod_ ->
      (* Debug file_watcher opens *)
      let is_file_watcher = String.contains (Module.path mod_) 'w' in
      if is_file_watcher then (
        Printf.printf "[DEBUG] Processing file_watcher, opens: [";
        List.iter
          (fun (n: dep Graph.node) ->
            match n.value.kind with
            | ML m
            | MLI m -> Printf.printf "%s, " (Module.namespaced_name m)
            | _ -> Printf.printf "?, ")
          dep.open_modules;
        Printf.printf "]\n"
      );
      let deps =
        match dep.file with
        | Generated _ -> []
        | Concrete _ -> Ocamldep.get_deps (Module.path mod_)
      in
      List.iter
        (fun dep ->
          (* printf "- %s\n" dep; *)
          let dep_name = Module_name.from_string dep in
          (* Debug IO dependencies *)
          if Module_name.to_string dep_name = "IO" then
            Printf.printf
              "[DEBUG] Resolving IO dep from %s\n"
              (
                match node.value.file with
                | Concrete p
                | Generated { path = p; _ } -> p
              );
          let resolved_via_aliases =
            List.concat_map
              (fun (alias_node: dep Graph.node) ->
                match alias_node.value.kind with
                | ML alias_mod
                | MLI alias_mod ->
                    (* Check if this is an aliases module *)
                    let alias_name = Module.namespaced_name alias_mod in
                    if String.ends_with ~suffix:"__Aliases" alias_name then
                      let namespace_prefix =
                        String.sub alias_name 0 (String.length alias_name - 9)
                      in
                      (* Remove "__Aliases" *)
                      let candidate_name =
                        namespace_prefix ^ "__" ^ Module_name.to_string dep_name
                      in
                      let is_debug =
                        Module_name.to_string dep_name = "Event"
                        || Module_name.to_string dep_name = "IO"
                      in
                      if is_debug then
                        Printf.printf
                          "[DEBUG]   Checking alias %s: looking for %s\n"
                          alias_name
                          candidate_name;
                    try
                      let node_ids = Module_registry.get_by_name t.registry dep_name in
                      if is_debug then
                        Printf.printf
                          "[DEBUG]   Found %d modules named %s in registry\n"
                          (List.length node_ids)
                          (Module_name.to_string dep_name);
                      let matching =
                        List.filter
                          (fun node_id ->
                            let candidate_node = Graph.get_node t.graph node_id in
                            match candidate_node.value.kind with
                            | ML m
                            | MLI m ->
                                let matches = Module.namespaced_name m = candidate_name in
                                if is_debug then
                                  Printf.printf
                                    "[DEBUG]     Candidate: %s (matches=%b)\n"
                                    (Module.namespaced_name m)
                                    matches;
                                matches
                            | _ -> false)
                          node_ids
                      in
                      if is_debug && List.length matching > 0 then
                        Printf.printf "[DEBUG]   Matched %d nodes\n" (List.length matching);
                      matching
                    with
                    | Not_found ->
                        if is_debug then
                          Printf.printf
                            "[DEBUG]   Module %s not found in registry\n"
                            (Module_name.to_string dep_name);
                          []
                        else
                          []
                    | _ -> [])
              node.value.open_modules
          in
          let is_debug =
            Module_name.to_string dep_name = "Event"
            || Module_name.to_string dep_name = "IO"
            || Module_name.to_string dep_name = "Global0"
          in
          match resolved_via_aliases with
          | [] ->
              (* Not found via aliases, fall back to registry search *)
              if is_debug then
                Printf.printf
                  "[DEBUG] %s NOT resolved via aliases, trying registry\n"
                  (Module_name.to_string dep_name);
              (
                try
                  let node_ids = Module_registry.get_by_name t.registry dep_name in
                  List.iter
                    (fun dep_node_id ->
                      let dep_node = Graph.get_node t.graph dep_node_id in
                      if is_debug then
                        Printf.printf
                          "[DEBUG] Found %s dep (registry): from %s to %s\n"
                          (Module_name.to_string dep_name)
                          (
                            match node.value.file with
                            | Concrete p
                            | Generated { path = p; _ } -> p
                          )
                          (
                            match dep_node.value.file with
                            | Concrete p
                            | Generated { path = p; _ } -> p
                          );
                      match (node.value.kind, dep_node.value.kind) with
                      | (MLI _, ML _) when has_matching_interface_surface t dep_node -> ()
                      | _ -> Graph.add_edge node ~depends_on:dep_node)
                    node_ids
                with
                | Not_found ->
                    if is_debug then
                      Printf.printf
                        "[DEBUG] %s NOT FOUND in registry! From file: %s\n"
                        (Module_name.to_string dep_name)
                        (
                          match node.value.file with
                          | Concrete p
                          | Generated { path = p; _ } -> p
                        )
              )
          | resolved_node_ids ->
              (* Found via aliases, use these specific modules *)
              List.iter
                (fun resolved_node_id ->
                  let dep_node = Graph.get_node t.graph resolved_node_id in
                  if is_debug then
                    Printf.printf
                      "[DEBUG] Resolved %s via aliases: from %s to %s\n"
                      (Module_name.to_string dep_name)
                      (
                        match node.value.file with
                        | Concrete p
                        | Generated { path = p; _ } -> p
                      )
                      (
                        match dep_node.value.file with
                        | Concrete p
                        | Generated { path = p; _ } -> p
                      );
                  (
                    match (node.value.kind, dep_node.value.kind) with
                    | (MLI _, ML _) when has_matching_interface_surface t dep_node -> ()
                    | _ -> Graph.add_edge node ~depends_on:dep_node
                  ))
                resolved_node_ids)
        deps
  | _ -> ()

let wire_deps = fun t ->
  (*
     printf "\n=== Module Registry before wiring deps ===\n";
     Module_registry.print t.registry;
     printf "=== End Registry ===\n\n";
  *)
  iter (handle_dep t) t

(* This function handles the special case of the root module of the package *)

let get_dependencies = fun t ->
  (* Get list of dependency package names from build_results *)
  (* These are all packages that were built before us *)
  let all_packages = Build_results.get_package_names t.build_results in
  List.filter (fun name -> name != Module_name.to_string t.package_name) all_packages

let scan_native_dir = fun t ->
  (* Also scan native/ directory for C/H files if it exists *)
  let native_dir = Filename.concat t.root Const.native_dir in
  if Sys.file_exists native_dir && Sys.is_directory native_dir then (
    printf "  Scanning native/ directory for C/H files\n";
    let files = Sys.readdir native_dir in
    Array.iter
      (fun filename ->
        let filepath = Filename.concat native_dir filename in
        if Sys.is_directory filepath then
          ()
        else
          let ext = Filename.extension filename in
          if ext = Const.c_ext then (
            (* Path relative to package root: packages/kernel/native/file.c *)
            let relative_path = t.root ^ "/" ^ Const.native_dir ^ "/" ^ filename in
            printf "    Found C file: %s\n" relative_path;
            (* Add as a standalone node - it will be picked up during iteration *)
            let node = { file = Concrete relative_path; open_modules = []; kind = C } in
            let _c_node = Graph.add_node t.graph node in
            ()
          ) else if ext = Const.h_ext then (
            (* Path relative to package root: packages/kernel/native/file.h *)
            let relative_path = t.root ^ "/" ^ Const.native_dir ^ "/" ^ filename in
            printf "    Found H file: %s\n" relative_path;
            let node = { file = Concrete relative_path; open_modules = []; kind = H } in
            let _h_node = Graph.add_node t.graph node in
            ()
          ))
      files
  )

let scan = fun ~root ~package ~build_results ->
  printf "Scanning package %S from %s\n" package.Package.name root;
  let t = make ~root ~package ~build_results in
  scan_from_root t;
  scan_native_dir t;
  wire_deps t;
  t
