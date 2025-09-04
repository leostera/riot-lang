(** Build planner module - converts Build_node.t to executable actions

    This module is responsible for taking a Build_node.t (which represents a
    package and its dependencies) and converting it into a concrete build plan
    with actions.

    The high-level flow is:

    1. INPUT: Build_node.t with package info and source files
    - Contains all .ml/.mli files for the package
    - Each source knows its simple name (e.g., "Cli") and namespaced name (e.g.,
      "Tusk__Cli")

    2. BUILD MODULE TREE (build_modtree):
    - Organize sources into a semantic tree structure: * Package (top-level) -
      represents the entire package
    - Has a kind: Library (builds .cma) or Binary (builds executable)
    - Has optional entry_point: main.ml for Binary, lib.ml for Library
    - Contains children: Library nodes (folders) and Module nodes (files)

    * Library (folder) - represents a folder in the source tree
    - Has a name (e.g., "cli") and namespaced_name (e.g., "Tusk__Cli")
    - May have folder_interface: the folder's interface file (e.g., cli/cli.ml)
    - Contains children: nested Library nodes and Module nodes

    * Module (leaf) - represents a single .ml/.mli file
    - Contains module_info: either Generated (for alias modules) or Concrete
      (actual files)

    3. INSERT ALIAS MODULES (insert_alias_modules):
    - Generate alias modules at each level to provide short names
    - Package level: generates Tusk__aliases with entries like "module Cli =
      Tusk__Cli"
    - Library level: generates Tusk__Cli__aliases with entries like "module
      Build = Tusk__Cli__Build"
    - Rules: * Only create aliases for modules where simple_name !=
      namespaced_name * Folder interfaces appear in parent's aliases, not their
      own folder's aliases * Each level only aliases its direct children

    4. SPLIT INTO THREE TREES (extract_alias_subtree, extract_intf_subtree,
    extract_impl_subtree):
    - Split the Mod_tree with aliases into three separate trees for proper
      compilation order: * Alias tree - contains only Generated modules (the
      __aliases.ml.gen files) * Interface tree - contains only modules with .mli
      files * Implementation tree - contains only modules with .ml files
    - This separation ensures correct compilation order: 1. Compile all alias
      modules first (they have no dependencies) 2. Compile all interfaces (may
      depend on aliases) 3. Compile all implementations (may depend on aliases
      and interfaces)

    5. ANALYZE DEPENDENCIES (analyze_dependencies_in_sandbox):
    - Write all files (including generated ones) to a temp sandbox
    - Run ocamldep to determine compilation order within each tree
    - Return sorted lists of .ml and .mli files

    6. GENERATE ACTIONS (module_tree_to_action_tree):
    - Process each tree in order (aliases, interfaces, implementations)
    - Convert each tree into concrete build actions: * WriteFile - for generated
      alias modules * CompileInterface - for .mli files * CompileImplementation
      \- for .ml files
    - Apply correct compilation flags: * -open flags for alias modules at each
      level * -I flags for include paths
    - Respect dependency order from ocamldep analysis within each tree

    7. OUTPUT: List of Actions.t that can be executed by the build system

    Key invariants:
    - Folder interfaces (e.g., cli/cli.ml) create a Library node with
      folder_interface set
    - The folder interface module (Cli) appears in the parent's aliases, not the
      folder's own aliases
    - Alias module names must be fully namespaced to avoid conflicts
      (Tusk__Cli__aliases not just Cli__aliases)
    - All generated content is deterministic for reproducible builds *)

open Std

(** Build result types *)
type skip_reason = DependenciesFailed of string list

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }
  | Skipped of { node : Build_node.t; reason : skip_reason }

type error = string

(** Step 1: Build a Mod_tree from sources *)
let build_modtree ~package_name ~srcs =
  Format.eprintf "[DEBUG] build_modtree: package_name=%s, srcs count=%d@."
    package_name (List.length srcs);
  let safe_package_name =
    String.map (fun c -> if c = '-' then '_' else c) package_name
  in

  (* Helper: organize sources by their path *)
  let sources_by_path = Hashtbl.create 32 in

  List.iter
    (fun source ->
      match source.Build_node.kind with
      | Build_node.ML { simple_name; namespaced_name; namespace } ->
          let path = String.concat "/" namespace in
          let key =
            if path = "" then simple_name else path ^ "/" ^ simple_name
          in
          Format.eprintf "[DEBUG]   ML: key=%s, simple=%s, namespaced=%s@." key
            simple_name namespaced_name;

          let existing =
            Hashtbl.find_opt sources_by_path key
            |> Option.value ~default:(simple_name, namespaced_name, None, None)
          in
          let s, n, _, intf = existing in
          Hashtbl.replace sources_by_path key (s, n, Some source, intf)
      | Build_node.MLI { simple_name; namespaced_name; namespace } ->
          let path = String.concat "/" namespace in
          let key =
            if path = "" then simple_name else path ^ "/" ^ simple_name
          in
          Format.eprintf "[DEBUG]   MLI: key=%s, simple=%s, namespaced=%s@." key
            simple_name namespaced_name;

          let existing =
            Hashtbl.find_opt sources_by_path key
            |> Option.value ~default:(simple_name, namespaced_name, None, None)
          in
          let s, n, impl, _ = existing in
          Hashtbl.replace sources_by_path key (s, n, impl, Some source)
      | _ -> ())
    srcs;

  (* Helper: build tree recursively *)
  let rec build_tree path =
    let prefix = if path = "" then "" else path ^ "/" in

    (* Find direct modules at this level *)
    let modules =
      Hashtbl.fold
        (fun key (simple, namespaced, impl, intf) acc ->
          if String.starts_with ~prefix key then
            let relative =
              String.sub key (String.length prefix)
                (String.length key - String.length prefix)
            in
            if not (String.contains relative '/') then
              let info =
                Mod_tree.Concrete
                  {
                    simple_name = simple;
                    namespaced_name = namespaced;
                    impl;
                    intf;
                  }
              in
              Mod_tree.Module info :: acc
            else acc
          else acc)
        sources_by_path []
    in

    (* Find subfolders at this level *)
    let subfolders =
      Hashtbl.fold
        (fun key _ acc ->
          if String.starts_with ~prefix key then
            let relative =
              String.sub key (String.length prefix)
                (String.length key - String.length prefix)
            in
            match String.index_opt relative '/' with
            | Some idx ->
                let folder = String.sub relative 0 idx in
                if not (List.mem folder acc) then folder :: acc else acc
            | None -> acc
          else acc)
        sources_by_path []
    in

    (* Build Library nodes for subfolders *)
    let libraries =
      List.map
        (fun folder ->
          let folder_path = if path = "" then folder else path ^ "/" ^ folder in
          let children = build_tree folder_path in

          (* Check for folder interface (e.g., cli/cli.ml) *)
          let folder_interface_key = folder_path ^ "/" ^ folder in
          let folder_interface =
            match Hashtbl.find_opt sources_by_path folder_interface_key with
            | Some (simple, namespaced, impl, intf) ->
                Some
                  (Mod_tree.Concrete
                     {
                       simple_name = simple;
                       namespaced_name = namespaced;
                       impl;
                       intf;
                     })
            | None -> None
          in

          (* Create Mod_name for this folder *)
          let base_namespace =
            Mod_name.namespace_of_list [ safe_package_name ]
          in
          let namespace =
            if path = "" then base_namespace
            else
              let path_namespace = Mod_name.namespace_of_string path in
              Mod_name.namespace_of_list
                (safe_package_name :: Mod_name.namespace_to_list path_namespace)
          in
          let folder_modname =
            Mod_name.of_filename ~namespace
              (Path.of_string folder
              |> Result.expect
                   ~msg:
                     (Printf.sprintf "Expected '%s' to be a valid Path" folder)
              )
          in

          Mod_tree.Library
            {
              name = folder_modname;
              folder_interface;
              children;
              aliases = [];
              (* will be filled by insert_alias_modules *)
            })
        subfolders
    in

    modules @ libraries
  in

  (* Determine package kind based on presence of main.ml *)
  let has_main = Hashtbl.mem sources_by_path "main" in
  let kind =
    if has_main then
      Mod_tree.Binary
        {
          src =
            Path.of_string "main.ml"
            |> Result.expect ~msg:"Expected 'main.ml' to be a valid Path";
          name = safe_package_name;
        }
    else Mod_tree.Library
  in

  (* Find entry point *)
  let entry_point =
    match kind with
    | Mod_tree.Binary _ -> (
        match Hashtbl.find_opt sources_by_path "main" with
        | Some (simple, namespaced, impl, intf) ->
            Some
              (Mod_tree.Concrete
                 {
                   simple_name = simple;
                   namespaced_name = namespaced;
                   impl;
                   intf;
                 })
        | None -> None)
    | Mod_tree.Library -> (
        match Hashtbl.find_opt sources_by_path safe_package_name with
        | Some (simple, namespaced, impl, intf) ->
            Some
              (Mod_tree.Concrete
                 {
                   simple_name = simple;
                   namespaced_name = namespaced;
                   impl;
                   intf;
                 })
        | None -> None)
  in

  (* Build the tree *)
  let children = build_tree "" in

  Mod_tree.Package
    {
      name = safe_package_name;
      kind;
      entry_point;
      children;
      aliases = [];
      (* will be filled by insert_alias_modules *)
    }

(** Step 2: Insert alias modules into the tree *)
let insert_alias_modules tree =
  let rec process = function
    | Mod_tree.Package { name; kind; entry_point; children; _ } ->
        (* Process children first *)
        let processed_children = List.map process children in

        (* Collect aliases for this level *)
        let aliases =
          List.filter_map
            (fun child ->
              match child with
              | Mod_tree.Library
                  { name; folder_interface = Some (Mod_tree.Concrete info); _ }
                ->
                  (* Folder with interface - add to parent's aliases *)
                  Format.eprintf
                    "[DEBUG]   Package alias: folder %s with interface %s -> \
                     %s@."
                    (Mod_name.qualified_name name)
                    info.simple_name info.namespaced_name;
                  if info.simple_name <> info.namespaced_name then
                    Some (info.simple_name, info.namespaced_name)
                  else None
              | Mod_tree.Module
                  (Mod_tree.Concrete { simple_name; namespaced_name; _ }) ->
                  (* Direct module - add to aliases if names differ *)
                  Format.eprintf "[DEBUG]   Package alias: module %s -> %s@."
                    simple_name namespaced_name;
                  if simple_name <> namespaced_name then
                    Some (simple_name, namespaced_name)
                  else None
              | _ -> None)
            processed_children
        in

        (* Generate alias module if needed *)
        let alias_modules =
          if aliases <> [] then
            let content =
              "(* Auto-generated module aliases *)\n"
              ^ String.concat "\n"
                  (List.map
                     (fun (simple, ns) ->
                       Printf.sprintf "module %s = %s" simple ns)
                     aliases)
            in
            let alias_name = String.capitalize_ascii name ^ "__aliases" in
            [
              Mod_tree.Generated
                {
                  simple_name = alias_name;
                  contents = content;
                  path = alias_name ^ ".ml.gen";
                  filename = alias_name ^ ".ml.gen";
                };
            ]
          else []
        in

        Mod_tree.Package
          {
            name;
            kind;
            entry_point;
            children = processed_children;
            aliases = alias_modules;
          }
    | Mod_tree.Library { name; folder_interface; children; _ } ->
        (* Process children first *)
        let processed_children = List.map process children in

        (* Collect aliases for this level - NOT including the folder interface itself *)
        let aliases =
          List.filter_map
            (fun child ->
              match child with
              | Mod_tree.Library
                  {
                    name = child_name;
                    folder_interface = Some (Mod_tree.Concrete info);
                    _;
                  } ->
                  (* Subfolder with interface *)
                  Format.eprintf
                    "[DEBUG]   Library %s alias: subfolder %s with interface \
                     %s -> %s@."
                    (Mod_name.qualified_name name)
                    (Mod_name.qualified_name child_name)
                    info.simple_name info.namespaced_name;
                  if info.simple_name <> info.namespaced_name then
                    Some (info.simple_name, info.namespaced_name)
                  else None
              | Mod_tree.Module
                  (Mod_tree.Concrete { simple_name; namespaced_name; _ }) ->
                  (* Direct module *)
                  Format.eprintf "[DEBUG]   Library %s alias: module %s -> %s@."
                    (Mod_name.qualified_name name)
                    simple_name namespaced_name;
                  if simple_name <> namespaced_name then
                    Some (simple_name, namespaced_name)
                  else None
              | _ -> None)
            processed_children
        in

        (* Generate alias module if needed *)
        let alias_modules =
          if aliases <> [] then
            let content =
              "(* Auto-generated module aliases *)\n"
              ^ String.concat "\n"
                  (List.map
                     (fun (simple, ns) ->
                       Printf.sprintf "module %s = %s" simple ns)
                     aliases)
            in
            let alias_name = Mod_name.qualified_name name ^ "__aliases" in
            [
              Mod_tree.Generated
                {
                  simple_name = alias_name;
                  contents = content;
                  path = alias_name ^ ".ml.gen";
                  filename = alias_name ^ ".ml.gen";
                };
            ]
          else []
        in

        Mod_tree.Library
          {
            name;
            folder_interface;
            children = processed_children;
            aliases = alias_modules;
          }
    | Mod_tree.Module _ as m -> m (* Leaf nodes unchanged *)
  in
  process tree

(** Step 3: Extract three subtrees for compilation ordering - preserving tree
    structure *)
let extract_alias_subtree tree =
  let rec extract_children children =
    List.filter_map
      (function
        | Mod_tree.Package _ -> None  (* Shouldn't have nested packages *)
        | Mod_tree.Library { name; folder_interface; children; aliases } ->
            (* Keep the library node but only with alias-related children *)
            let child_trees = extract_children children in
            (* Only keep this library if it has aliases or alias-containing children *)
            if aliases <> [] || child_trees <> [] then
              Some (Mod_tree.Library
                {
                  name;
                  folder_interface = None;
                  children = child_trees;
                  aliases;
                })
            else None
        | Mod_tree.Module _ ->
            (* Regular modules don't belong in the alias tree *)
            None)
      children
  in
  match tree with
  | Mod_tree.Package { name; kind; entry_point; children; aliases } ->
      (* Keep the package node but only with alias-related children *)
      let child_trees = extract_children children in
      Mod_tree.Package
        {
          name;
          kind;
          entry_point = None;
          children = child_trees;
          aliases;
        }
  | _ -> tree  (* Should be a Package at the root *)

let extract_intf_subtree tree =
  let rec extract = function
    | Mod_tree.Package { name; kind; entry_point; children; aliases } ->
        (* Keep the package node but only with interface-bearing children *)
        let child_trees =
          List.filter_map
            (fun child ->
              match extract child with
              | Mod_tree.Package { children = []; _ } -> None
              | Mod_tree.Library { children = []; folder_interface = None; _ }
                ->
                  None
              | tree -> Some tree)
            children
        in
        Mod_tree.Package
          {
            name;
            kind;
            entry_point = None;
            children = child_trees;
            aliases;  (* Keep aliases in interface tree *)
          }
    | Mod_tree.Library { name; folder_interface; children; aliases } ->
        (* Keep folder interface if it has an interface *)
        let intf =
          match folder_interface with
          | Some (Mod_tree.Concrete { intf = Some _; _ } as info) -> Some info
          | _ -> None
        in
        let child_trees =
          List.filter_map
            (fun child ->
              match extract child with
              | Mod_tree.Package { children = []; _ } -> None
              | Mod_tree.Library { children = []; folder_interface = None; _ }
                ->
                  None
              | tree -> Some tree)
            children
        in
        Mod_tree.Library
          {
            name;
            folder_interface = intf;
            children = child_trees;
            aliases;  (* Keep aliases in interface tree *)
          }
    | Mod_tree.Module (Mod_tree.Concrete { intf = Some intf_src; namespaced_name; simple_name; _ }) ->
        (* Create interface-only node *)
        Mod_tree.Module (Mod_tree.Concrete { 
          impl = None; 
          intf = Some intf_src; 
          namespaced_name; 
          simple_name 
        })
    | _ ->
        Mod_tree.Module
          (Mod_tree.Generated
             { simple_name = ""; contents = ""; path = ""; filename = "" })
  in
  extract tree

let extract_impl_subtree tree =
  let rec extract = function
    | Mod_tree.Package { name; kind; entry_point; children; aliases } ->
        (* Keep entry point if it has implementation *)
        let entry =
          match entry_point with
          | Some (Mod_tree.Concrete { impl = Some _; _ } as info) -> Some info
          | _ -> None
        in
        let child_trees =
          List.filter_map
            (fun child ->
              match extract child with
              | Mod_tree.Package { children = []; entry_point = None; aliases = [] } ->
                  None
              | Mod_tree.Library { children = []; folder_interface = None; aliases = [] }
                ->
                  None
              | tree -> Some tree)
            children
        in
        Mod_tree.Package
          {
            name;
            kind;
            entry_point = entry;
            children = child_trees;
            aliases;  (* Keep aliases in impl tree *)
          }
    | Mod_tree.Library { name; folder_interface; children; aliases } ->
        (* Keep folder interface if it has an implementation *)
        let intf =
          match folder_interface with
          | Some (Mod_tree.Concrete { impl = Some _; _ } as info) -> Some info
          | _ -> None
        in
        let child_trees =
          List.filter_map
            (fun child ->
              match extract child with
              | Mod_tree.Package { children = []; entry_point = None; aliases = [] } ->
                  None
              | Mod_tree.Library { children = []; folder_interface = None; aliases = [] }
                ->
                  None
              | tree -> Some tree)
            children
        in
        Mod_tree.Library
          {
            name;
            folder_interface = intf;
            children = child_trees;
            aliases;  (* Keep aliases in impl tree *)
          }
    | Mod_tree.Module (Mod_tree.Concrete { impl = Some impl_src; namespaced_name; simple_name; _ }) ->
        (* Create implementation-only node *)
        Mod_tree.Module (Mod_tree.Concrete { 
          impl = Some impl_src; 
          intf = None; 
          namespaced_name; 
          simple_name 
        })
    | _ ->
        Mod_tree.Module
          (Mod_tree.Generated
             { simple_name = ""; contents = ""; path = ""; filename = "" })
  in
  extract tree

(** Compute hash for a planned node *)
let compute_hash_for_planned_node ~toolchain ~package ~srcs ~deps ~outs ~actions
    =
  (* Collect all the hash seeds *)
  let seeds = ref [] in

  (* Hash the toolchain *)
  let toolchain_hash = Toolchains.hash toolchain in
  seeds := Hasher.to_string toolchain_hash :: !seeds;

  (* Add package name *)
  seeds := package.Workspace.name :: !seeds;

  (* Hash source files - extract the file paths from source records *)
  let src_files = List.map (fun s -> s.Build_node.file) srcs in
  let srcs_hash = Hasher.hash_files src_files in
  seeds := Hasher.to_string srcs_hash :: !seeds;

  (* Add dependency hashes *)
  let dep_nodes = deps in
  (* deps are already nodes, not IDs *)
  List.iter
    (fun dep ->
      match dep.Build_node.spec with
      | Unplanned -> ()
      | Planned { hash; _ } -> seeds := Hasher.to_string hash :: !seeds)
    dep_nodes;

  (* Hash actions *)
  let actions_hash = Actions.hash actions in
  seeds := Hasher.to_string actions_hash :: !seeds;

  (* Combine all seeds into final hash *)
  Hasher.hash_strings (List.rev !seeds)

(** Step 5: Generate actions from module trees using Dep_set for ordering *)
let module_trees_to_actions_v2 ~toolchain ~package ~alias_modules ~intf_modules ~impl_modules =
  let actions = ref [] in

  (* Determine the main alias module name for -open flag *)
  let safe_package_name =
    String.map (fun c -> if c = '-' then '_' else c) package.Workspace.name
  in
  let main_alias_module =
    String.capitalize_ascii safe_package_name ^ "__aliases"
  in
  let open_flags =
    if Dep_set.size alias_modules > 0 then [ Ocamlc.Open main_alias_module ]
    else []
  in

  (* Phase 1: Process alias modules (no dependencies, no ocamldep needed) *)
  Format.eprintf "[DEBUG] Processing alias modules...@.";

  Dep_set.iter
    (function
      | Mod_tree.Module
          (Mod_tree.Generated { filename; contents; simple_name; _ }) ->
          (* Write the file *)
          Format.eprintf "[DEBUG]   WriteFile: %s@." filename;
          actions :=
            Actions.WriteFile { destination = filename; content = contents }
            :: !actions;

          (* Compile interface and implementation *)
          let impl_path =
            Path.of_string filename
            |> Result.expect
                 ~msg:
                   (Printf.sprintf "Expected '%s' to be a valid Path" filename)
          in
          let modname =
            Mod_name.make ~filename:impl_path
              ~namespace:(Mod_name.namespace_of_list [])
              ~name:simple_name
          in

          let cmi_output = Mod_name.cmi modname in
          Format.eprintf "[DEBUG]   CompileInterface: %s -> %s@." filename
            cmi_output;
          actions :=
            Actions.CompileInterface
              {
                source = filename;
                output = cmi_output;
                includes = [ "." ];
                flags =
                  [
                    Ocamlc.NoAliasDeps;
                    Ocamlc.Impl impl_path;
                    Ocamlc.Warning [ Ocamlc.NoCmiFile ];
                  ];
              }
            :: !actions;

          let cmo_output = Mod_name.cmo modname in
          Format.eprintf "[DEBUG]   CompileImplementation: %s -> %s@." filename
            cmo_output;
          actions :=
            Actions.CompileImplementation
              {
                source = filename;
                output = cmo_output;
                includes = [ "." ];
                flags =
                  [
                    Ocamlc.NoAliasDeps;
                    Ocamlc.Impl impl_path;
                    Ocamlc.Warning [ Ocamlc.NoCmiFile ];
                  ];
              }
            :: !actions
      | _ -> ())
    alias_modules;

  (* Phase 2: Process interface modules in dependency order *)
  Format.eprintf "[DEBUG] Processing interface modules...@.";

  Dep_set.iter
    (function
      | Mod_tree.Module
          (Mod_tree.Concrete { intf = Some src; namespaced_name; _ }) ->
          let original_path = Path.to_string src.Build_node.file in
          let source_name = Filename.basename original_path in
          let output = namespaced_name ^ ".cmi" in
          
          (* First copy the source file to sandbox *)
          actions :=
            Actions.CopyFile
              {
                source = original_path;
                destination = source_name;
              }
            :: !actions;
          
          Format.eprintf "[DEBUG]   CompileInterface: %s -> %s@." source_name
            output;
          actions :=
            Actions.CompileInterface
              {
                source = source_name;
                output;
                includes = [ "." ];
                flags = open_flags;
              }
            :: !actions
      | _ -> ())
    intf_modules;

  (* Phase 3: Process implementation modules in dependency order *)
  Format.eprintf "[DEBUG] Processing implementation modules...@.";

  (* Keep track of .cmo files in dependency order for linking *)
  let cmo_files = ref [] in

  Dep_set.iter
    (function
      | Mod_tree.Module
          (Mod_tree.Concrete { impl = Some src; namespaced_name; intf; _ }) ->
          let original_path = Path.to_string src.Build_node.file in
          let source_name = Filename.basename original_path in
          
          (* First copy the source file to sandbox *)
          actions :=
            Actions.CopyFile
              {
                source = original_path;
                destination = source_name;
              }
            :: !actions;

          (* If no interface file exists, we need to generate .cmi from .ml *)
          (match intf with
          | None ->
              let cmi_output = namespaced_name ^ ".cmi" in
              Format.eprintf "[DEBUG]   CompileInterface from .ml: %s -> %s@."
                source_name cmi_output;
              actions :=
                Actions.CompileInterface
                  {
                    source = source_name;
                    output = cmi_output;
                    includes = [ "." ];
                    flags = open_flags;
                  }
                :: !actions
          | Some _ -> ());

          let cmo_output = namespaced_name ^ ".cmo" in
          Format.eprintf "[DEBUG]   CompileImplementation: %s -> %s@."
            source_name cmo_output;
          actions :=
            Actions.CompileImplementation
              {
                source = source_name;
                output = cmo_output;
                includes = [ "." ];
                flags = open_flags;
              }
            :: !actions;
          cmo_files := cmo_output :: !cmo_files
      | _ -> ())
    impl_modules;

  (* Phase 3: Generate Link action if this is a library or binary *)
  (* The cmo_files are already in dependency order from Dep_set iteration *)
  let all_objects = List.rev !cmo_files in

  (* Determine if this is a binary or library *)
  let has_main =
    List.exists
      (function
        | Actions.CompileImplementation { source; _ } ->
            String.ends_with ~suffix:"main.ml" source
            || String.ends_with ~suffix:"Main.ml" source
        | _ -> false)
      !actions
  in

  if all_objects <> [] then (
    if has_main then (
      (* Binary *)
      let output = package.Workspace.name in
      Format.eprintf "[DEBUG]   CreateExecutable: %s@." output;
      Format.eprintf "[DEBUG]   Link order: %s@."
        (String.concat ", " all_objects);
      actions :=
        Actions.CreateExecutable
          {
            output;
            objects = all_objects;
            libraries = [];
            (* TODO: Add dependencies *)
            includes = [ "." ];
          }
        :: !actions)
    else
      (* Library *)
      let output = safe_package_name ^ ".cma" in
      Format.eprintf "[DEBUG]   CreateLibrary: %s@." output;
      Format.eprintf "[DEBUG]   Link order: %s@."
        (String.concat ", " all_objects);
      actions :=
        Actions.CreateLibrary
          { output; objects = all_objects; includes = [ "." ] }
        :: !actions);

  List.rev !actions

(** Main entry point using the new Dep_set-based approach *)
let plan_node ~graph ~node ~build_results ~session_id () =
  (* Check if node is already planned *)
  match node.Build_node.spec with
  | Build_node.Planned _ -> Ok (Planned node)
  | Build_node.Unplanned ->
      (* Check dependencies *)
      let missing_deps =
        List.filter
          (fun dep ->
            let dep_node = Build_graph.get_node graph dep in
            match
              Build_results.get_status build_results
                dep_node.Build_node.package.name
            with
            | Some (Build_results.Built _hash) -> false
            | _ -> true)
          node.deps
      in

      if missing_deps <> [] then
        let dep_nodes = List.map (Build_graph.get_node graph) missing_deps in
        Ok (MissingDependencies { node; deps = dep_nodes })
      else
        (* All dependencies are ready, plan this node *)
        let package = node.Build_node.package in
        let toolchain = node.toolchain in

        (* Step 1: Build module tree from sources *)
        let package_name = package.Workspace.name in
        let tree = build_modtree ~package_name ~srcs:node.srcs in

        Format.eprintf "[DEBUG] Tree after build_modtree:@.";
        Mod_tree.print tree;

        (* Step 2: Insert alias modules *)
        let tree_with_aliases = insert_alias_modules tree in

        Format.eprintf "[DEBUG] Tree after insert_alias_modules:@.";
        Mod_tree.print tree_with_aliases;

        (* Step 3: Extract three subtrees for proper compilation order *)
        let alias_tree = extract_alias_subtree tree_with_aliases in
        let intf_tree = extract_intf_subtree tree_with_aliases in
        let impl_tree = extract_impl_subtree tree_with_aliases in

        Format.eprintf "[DEBUG] ===== EXTRACTING THREE SUBTREES =====@.";
        Format.eprintf "[DEBUG] Extracted alias tree:@.";
        Mod_tree.print alias_tree;
        Format.eprintf "[DEBUG] Extracted intf tree:@.";
        Mod_tree.print intf_tree;
        Format.eprintf "[DEBUG] Extracted impl tree:@.";
        Mod_tree.print impl_tree;

        (* Step 4: Create Dep_sets for each tree *)
        let alias_depset =
          Dep_set.create
            ~name:(package_name ^ "_aliases")
            ~toolchain 
            ~tree:alias_tree
        in
        let intf_depset =
          Dep_set.create
            ~name:(package_name ^ "_interfaces")
            ~toolchain 
            ~tree:intf_tree
        in
        let impl_depset =
          Dep_set.create
            ~name:(package_name ^ "_implementations")
            ~toolchain 
            ~tree:impl_tree
        in

        (* Step 5: Generate actions from the Dep_sets *)
        let actions =
          module_trees_to_actions_v2 ~toolchain ~package
            ~alias_modules:alias_depset
            ~intf_modules:intf_depset
            ~impl_modules:impl_depset
        in

        Format.eprintf "[DEBUG] Generated %d actions@." (List.length actions);

        (* Get dependency nodes for hash computation *)
        let dep_nodes = List.map (Build_graph.get_node graph) node.deps in

        (* Compute hash using the comprehensive function *)
        let hash =
          compute_hash_for_planned_node ~toolchain ~package ~srcs:node.srcs
            ~deps:dep_nodes ~outs:[] (* TODO: compute outputs *)
            ~actions
        in

        (* Collect all output files from actions to declare *)
        let output_files =
          List.filter_map
            (fun action ->
              match action with
              | Actions.CompileInterface { output; _ } -> Some output
              | Actions.CompileImplementation { output; _ } -> Some output
              | Actions.CreateLibrary { output; _ } -> Some output
              | Actions.CreateExecutable { output; _ } -> Some output
              | _ -> None)
            actions
        in
        
        (* Add DeclareOutputs action if we have outputs *)
        let actions =
          if output_files <> [] then
            Actions.DeclareOutputs { outputs = output_files } :: actions
          else
            actions
        in
        
        (* Convert output strings to paths for the outs field *)
        let outs =
          List.map
            (fun output ->
              Path.of_string output
              |> Result.expect ~msg:(Printf.sprintf "Expected %s to be a valid path" output))
            output_files
        in

        (* Create planned node *)
        let planned_node =
          {
            node with
            Build_node.spec =
              Build_node.Planned
                {
                  hash;
                  outs;
                  actions;
                };
          }
        in

        Ok (Planned planned_node)
