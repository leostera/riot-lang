open Std

type skip_reason = DependenciesFailed of string list

(** Tree structure for organizing modules by folders *)
type module_tree = {
  name: string;  (* Module or folder name *)
  impl: Build_node.source option;  (* .ml file if exists *)
  intf: Build_node.source option;  (* .mli file if exists *)
  root: Path.t;  (* Directory path *)
  children: module_tree list;  (* Sub-modules or sub-folders *)
}

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }
  | Skipped of { node : Build_node.t; reason : skip_reason }

type error = string

(** Convert module tree to JSON string for debugging *)
let rec module_tree_to_json tree =
  let escape_string s = "\"" ^ String.escaped s ^ "\"" in
  let impl_json = match tree.impl with
    | Some impl -> (match impl.Build_node.kind with
        | Build_node.ML { simple_name; namespaced_name; namespace; _ } ->
            Printf.sprintf "{\"type\":\"ML\",\"simple_name\":%s,\"namespaced_name\":%s,\"namespace\":[%s]}"
              (escape_string simple_name) (escape_string namespaced_name)
              (String.concat "," (List.map escape_string namespace))
        | _ -> "null")
    | None -> "null"
  in
  let intf_json = match tree.intf with
    | Some intf -> (match intf.Build_node.kind with
        | Build_node.MLI { simple_name; namespaced_name; namespace; _ } ->
            Printf.sprintf "{\"type\":\"MLI\",\"simple_name\":%s,\"namespaced_name\":%s,\"namespace\":[%s]}"
              (escape_string simple_name) (escape_string namespaced_name)
              (String.concat "," (List.map escape_string namespace))
        | _ -> "null")
    | None -> "null"
  in
  let children_json = "[" ^ String.concat "," (List.map module_tree_to_json tree.children) ^ "]" in
  Printf.sprintf "{\"name\":%s,\"impl\":%s,\"intf\":%s,\"children\":%s}"
    (escape_string tree.name) impl_json intf_json children_json

(** Build a module tree from a list of sources *)
let build_module_tree ~package_name ~srcs =
  let safe_package_name = String.map (fun c -> if c = '-' then '_' else c) package_name in
  
  (* First, organize sources into a map by path *)
  let path_map = Hashtbl.create 32 in
  
  List.iter (fun source ->
    match source.Build_node.kind with
    | Build_node.ML { simple_name; namespace; _ } ->
        (* Use simple_name as key for top-level modules to avoid collisions *)
        let path_key = if namespace = [] then simple_name else String.concat "/" namespace in
        let existing = Hashtbl.find_opt path_map path_key |> Option.value ~default:(simple_name, None, None) in
        let (name, _, intf) = existing in
        Hashtbl.replace path_map path_key (name, Some source, intf)
    | Build_node.MLI { simple_name; namespace; _ } ->
        (* Use simple_name as key for top-level modules to avoid collisions *)
        let path_key = if namespace = [] then simple_name else String.concat "/" namespace in
        let existing = Hashtbl.find_opt path_map path_key |> Option.value ~default:(simple_name, None, None) in
        let (name, impl, _) = existing in
        Hashtbl.replace path_map path_key (name, impl, Some source)
    | _ -> ()
  ) srcs;
  
  (* Now build the tree structure *)
  let rec build_subtree current_path =
    (* Find all direct children of current_path *)
    let direct_children = Hashtbl.fold (fun path (name, impl, intf) acc ->
      if current_path = "" then
        (* At root - find modules with no namespace *)
        if not (String.contains path '/') then
          (* This is a root module (simple_name as key) or a folder *)
          if impl <> None || intf <> None then
            (* This is a root module with implementation or interface *)
            { name; impl; intf; 
              root = Path.of_string "." |> Result.expect ~msg:"Invalid path '.'"; 
              children = [] } :: acc
          else
            (* This might be a folder at root level *)
            let folder_modules = build_subtree path in
            { name = path; impl = None; intf = None;
              root = Path.of_string path |> Result.expect ~msg:("Invalid path: " ^ path);
              children = folder_modules } :: acc
        else acc
      else
        (* In a folder - find modules in this folder *)
        if String.starts_with ~prefix:(current_path ^ "/") path then
          let suffix = String.sub path (String.length current_path + 1) 
                        (String.length path - String.length current_path - 1) in
          if not (String.contains suffix '/') then
            (* Direct child of this folder *)
            { name; impl; intf;
              root = Path.of_string path |> Result.expect ~msg:("Invalid path: " ^ path);
              children = [] } :: acc
          else acc
        else if path = current_path then
          (* This is the folder's own module (e.g., cli/cli.ml) *)
          { name; impl; intf;
            root = Path.of_string current_path |> Result.expect ~msg:("Invalid path: " ^ current_path);
            children = [] } :: acc
        else acc
    ) path_map [] in
    direct_children
  in
  
  (* Build from root *)
  let children = build_subtree "" in
  
  (* Find the package's own module if it exists *)
  let pkg_impl = ref None in
  let pkg_intf = ref None in
  List.iter (fun source ->
    match source.Build_node.kind with
    | Build_node.ML { simple_name; namespace; _ } when namespace = [] && simple_name = String.capitalize_ascii safe_package_name ->
        pkg_impl := Some source
    | Build_node.MLI { simple_name; namespace; _ } when namespace = [] && simple_name = String.capitalize_ascii safe_package_name ->
        pkg_intf := Some source
    | _ -> ()
  ) srcs;
  
  {
    name = safe_package_name;
    impl = !pkg_impl;
    intf = !pkg_intf;
    root = (match srcs with 
           | [] -> Path.of_string "." |> Result.expect ~msg:"Invalid path '.'"
           | hd :: _ -> 
               let dir = Filename.dirname (Path.to_string hd.Build_node.file) in
               Path.of_string dir |> Result.expect ~msg:("Invalid path: " ^ dir));
    children = children;
  }

let get_dependency_libs_and_includes toolchain dependencies =
  let libs = ref [] in
  let includes = ref [] in
  List.iter
    (fun (dep : Workspace.dependency) ->
      match dep.Workspace.name with
      | "unix" ->
          libs := "unix.cma" :: !libs;
          includes := "+unix" :: !includes
      | "threads" ->
          libs := "threads.cma" :: !libs;
          includes := "+threads" :: !includes
      | _ -> ())
    dependencies;
  (!libs, !includes)

(* Check if any transitive dependency requires unix *)
let needs_unix_transitively ~graph ~node =
  let rec check_node visited current_node =
    if List.mem current_node.Build_node.package.name visited then false
    else
      let visited = current_node.Build_node.package.name :: visited in
      (* Check if this node directly depends on unix *)
      let has_unix_dep =
        List.exists
          (fun (dep : Workspace.dependency) -> dep.Workspace.name = "unix")
          current_node.Build_node.package.dependencies
      in
      if has_unix_dep then true
      else
        (* Check transitive dependencies *)
        let dep_nodes =
          List.map (Build_graph.get_node graph) current_node.Build_node.deps
        in
        List.exists (check_node visited) dep_nodes
  in
  check_node [] node

(** Generate actions from a module tree - recursive approach *)
let rec generate_actions_from_tree ~tree ~package ~dep_includes ~parent_aliases ~level ~actions ~outputs =
  let safe_package_name = String.map (fun c -> if c = '-' then '_' else c) package.Workspace.name in
  
  (* Generate alias module for this level if it has children *)
  let alias_module_opt, open_flags = 
    if tree.children <> [] then
      (* Collect aliases for modules at this level ONLY *)
      let module_aliases = 
        List.filter_map (fun child ->
          match child.impl, child.intf with
          | Some impl, _ ->
              (match impl.Build_node.kind with
               | Build_node.ML { simple_name; namespaced_name; _ } ->
                   (* Only include direct children, not nested modules *)
                   (* At package level: only top-level files *)
                   (* At folder level: only files directly in this folder *)
                   if simple_name <> tree.name then (* Exclude folder interface modules *)
                     Some (simple_name, namespaced_name)
                   else None
               | _ -> None)
          | None, Some intf ->
              (match intf.Build_node.kind with
               | Build_node.MLI { simple_name; namespaced_name; _ } ->
                   (* Only include direct children, not nested modules *)
                   if simple_name <> tree.name then (* Exclude folder interface modules *)
                     Some (simple_name, namespaced_name)
                   else None
               | _ -> None)
          | None, None when child.children <> [] ->
              (* This is a subfolder - only include at package level *)
              if level = 0 then (
                let folder_name = String.capitalize_ascii child.name in
                let namespaced_folder = String.capitalize_ascii safe_package_name ^ "__" ^ folder_name in
                Some (folder_name, namespaced_folder)
              ) else None
          | _ -> None
        ) tree.children
      in
      
      if module_aliases <> [] then
        (* Generate alias module name based on level *)
        let alias_module_name = 
          if level = 0 then
            String.capitalize_ascii safe_package_name ^ "__aliases"
          else
            (* Folder-level alias module *)
            let folder_namespace = 
              if tree.name = safe_package_name then ""
              else String.capitalize_ascii safe_package_name ^ "__" ^ String.capitalize_ascii tree.name
            in
            folder_namespace ^ "__aliases"
        in
        
        let alias_content = 
          "(* Auto-generated module aliases for " ^ 
          (if level = 0 then "package " ^ package.Workspace.name else "folder " ^ tree.name) ^ 
          " *)\n" ^
          (module_aliases
           |> List.map (fun (simple, namespaced) ->
               Printf.sprintf "module %s = %s" simple namespaced)
           |> String.concat "\n")
        in
        
        let alias_ml = alias_module_name ^ ".ml" in
        let alias_cmo = alias_module_name ^ ".cmo" in
        let alias_cmi = alias_module_name ^ ".cmi" in
        
        (* Add alias module compilation actions *)
        actions := Actions.WriteFile { destination = alias_ml; content = alias_content } :: !actions;
        actions := Actions.CompileImplementation 
          { source = alias_ml; output = alias_cmo; 
            includes = dep_includes;
            flags = [Ocamlc.NoAliasDeps] } :: !actions;
        outputs := alias_cmi :: alias_cmo :: !outputs;
        
        (Some (alias_module_name, alias_content), [Ocamlc.Open alias_module_name])
      else
        (None, parent_aliases |> Option.map (fun (name, _) -> [Ocamlc.Open name]) |> Option.value ~default:[])
    else
      (None, parent_aliases |> Option.map (fun (name, _) -> [Ocamlc.Open name]) |> Option.value ~default:[])
  in
  
  (* Compile this level's interface if it exists *)
  (match tree.intf with
   | Some intf_source ->
       let intf_str = Path.to_string intf_source.Build_node.file in
       let cmi_path = 
         match intf_source.Build_node.kind with
         | Build_node.MLI { namespaced_name; _ } -> namespaced_name ^ ".cmi"
         | _ -> failwith "Internal error: non-MLI source"
       in
       actions :=
         Actions.CompileInterface
           { source = intf_str; output = cmi_path; includes = dep_includes; flags = open_flags }
         :: !actions;
       outputs := cmi_path :: !outputs
   | None -> ());
  
  (* Compile this level's implementation if it exists *)
  (match tree.impl with
   | Some impl_source ->
       let impl_str = Path.to_string impl_source.Build_node.file in
       let cmo_path = 
         match impl_source.Build_node.kind with
         | Build_node.ML { namespaced_name; _ } -> namespaced_name ^ ".cmo"
         | _ -> failwith "Internal error: non-ML source"
       in
       actions :=
         Actions.CompileImplementation
           { source = impl_str; output = cmo_path; includes = dep_includes; flags = open_flags }
         :: !actions;
       outputs := cmo_path :: !outputs
   | None -> ());
  
  (* Recursively process children *)
  List.iter (fun child ->
    generate_actions_from_tree ~tree:child ~package ~dep_includes 
      ~parent_aliases:alias_module_opt ~level:(level + 1) ~actions ~outputs
  ) tree.children

let generate_actions ~graph ~node ~toolchain ~package ~srcs ~deps =
  let actions = ref [] in
  let outputs = ref [] in
  (* List of output file paths as strings *)

  (* Get external dependencies from package *)
  let external_libs, external_includes =
    get_dependency_libs_and_includes toolchain package.Workspace.dependencies
  in

  (* Get dependency include paths from local packages *)
  (* Since all dependency artifacts are copied to the sandbox directory,
     we just need to include the current directory (.) where they'll be available *)
  let local_dep_includes =
    if deps <> [] then [ "." ]
      (* Sandbox directory contains all dependency artifacts *)
    else []
  in

  (* Check if we need unix transitively for includes too *)
  let needs_unix_trans = needs_unix_transitively ~graph ~node in
  let all_external_includes =
    if needs_unix_trans && not (List.mem "+unix" external_includes) then
      "+unix" :: external_includes
    else external_includes
  in
  (* Combine all include paths *)
  let dep_includes = all_external_includes @ local_dep_includes in

  (* Separate source files by type using the kind field *)
  let ml_sources =
    List.filter (fun s -> match s.Build_node.kind with Build_node.ML _ -> true | _ -> false) srcs
  in
  let mli_sources =
    List.filter (fun s -> match s.Build_node.kind with Build_node.MLI _ -> true | _ -> false) srcs
  in
  let c_sources =
    List.filter (fun s -> match s.Build_node.kind with Build_node.C_stub -> true | _ -> false) srcs
  in

  (* Sort ML and MLI files in dependency order using ocamldep *)
  let sorted_ml_sources, sorted_mli_sources =
    if ml_sources <> [] || mli_sources <> [] then
      (* Get the directory containing the source files *)
      let src_dir =
        match srcs with
        | [] -> "."
        | hd :: _ -> Filename.dirname (Std.Path.to_string hd.Build_node.file)
      in
      (* Convert to basenames for ocamldep *)
      let ml_basenames =
        List.map (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file)) ml_sources
      in
      let mli_basenames =
        List.map (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file)) mli_sources
      in
      let all_basenames = mli_basenames @ ml_basenames in

      let sorted_basenames =
        Ocamldep.sort ~toolchain ~cwd:src_dir ~files:all_basenames
      in

      (* Map sorted basenames back to source records *)
      let basename_to_source lst =
        List.filter_map (fun basename ->
            List.find_opt
              (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file) = basename)
              lst)
      in

      let sorted_mli =
        List.filter (fun f -> Filename.check_suffix f ".mli") sorted_basenames
        |> fun names -> basename_to_source mli_sources names
      in
      let sorted_ml =
        List.filter (fun f -> Filename.check_suffix f ".ml") sorted_basenames
        |> fun names -> basename_to_source ml_sources names
      in
      (sorted_ml, sorted_mli)
    else (ml_sources, mli_sources)
  in

  (* Build module tree from source paths *)
  let tree = build_module_tree ~package_name:package.Workspace.name ~srcs in
  
  (* Recursively generate alias files for each directory level *)
  let rec generate_hierarchical_aliases ~tree ~package_name ~namespace_prefix ~actions ~outputs ~dep_includes =
    let safe_package_name = String.map (fun c -> if c = '-' then '_' else c) package_name in
    
    (* Generate alias module name for this directory level *)
    let alias_module_name = 
      if namespace_prefix = "" then
        String.capitalize_ascii safe_package_name ^ "__aliases"
      else
        namespace_prefix ^ "__aliases"
    in
    
    (* If this is a subdirectory (not the root), also generate a wrapper module *)
    if namespace_prefix <> "" then (
      let wrapper_module_name = namespace_prefix ^ "__wrapper" in
      let wrapper_ml = wrapper_module_name ^ ".ml" in
      let wrapper_cmi = wrapper_module_name ^ ".cmi" in
      let wrapper_cmo = wrapper_module_name ^ ".cmo" in
      
      (* Generate an empty wrapper module like dune does *)
      let wrapper_content = 
        "(* Auto-generated wrapper module for subdirectory *)\n" ^
        "module " ^ wrapper_module_name ^ " = struct end\n" ^
        "[@@deprecated \"this module is shadowed\"]"
      in
      
      (* Add actions to write and compile the wrapper module *)
      actions := Actions.WriteFile { destination = wrapper_ml; content = wrapper_content } :: !actions;
      outputs := wrapper_ml :: !outputs;
      
      (* Compile wrapper interface first (generate empty .mli) *)
      let wrapper_mli = wrapper_module_name ^ ".mli" in
      let wrapper_mli_content = "(* Auto-generated wrapper interface *)" in
      actions := Actions.WriteFile { destination = wrapper_mli; content = wrapper_mli_content } :: !actions;
      outputs := wrapper_mli :: !outputs;
      
      actions := Actions.CompileInterface
        { source = wrapper_mli; output = wrapper_cmi; includes = dep_includes; flags = [] }
        :: !actions;
      outputs := wrapper_cmi :: !outputs;
      
      (* Compile wrapper implementation *)
      actions := Actions.CompileImplementation
        { source = wrapper_ml; output = wrapper_cmo; includes = dep_includes; flags = [] }
        :: !actions;
      outputs := wrapper_cmo :: !outputs
    );
    
    (* Collect aliases for direct children only *)
    let direct_child_aliases =
      List.filter_map (fun child ->
        match child.impl, child.intf, child.children with
        | Some impl, _, [] ->
            (* Direct child file (not folder) with implementation *)
            (match impl.Build_node.kind with
             | Build_node.ML { simple_name; namespaced_name; _ } ->
                 if simple_name <> namespaced_name then Some (simple_name, namespaced_name) else None
             | _ -> None)
        | None, Some intf, [] ->
            (* Direct child file (not folder) with only interface *)
            (match intf.Build_node.kind with
             | Build_node.MLI { simple_name; namespaced_name; _ } ->
                 if simple_name <> namespaced_name then Some (simple_name, namespaced_name) else None
             | _ -> None)
        | _, _, _ :: _ ->
            (* This is a folder - create alias to folder interface module *)
            let folder_name = String.capitalize_ascii child.name in
            let folder_namespace = 
              if namespace_prefix = "" then
                String.capitalize_ascii safe_package_name ^ "__" ^ folder_name
              else
                namespace_prefix ^ "__" ^ folder_name
            in
            Some (folder_name, folder_namespace)
        | None, None, [] ->
            (* Empty node - skip *)
            None
      ) tree.children
    in
    
    (* Generate alias file content *)
    let comment = 
      if namespace_prefix = "" then
        "(* Auto-generated module aliases for package " ^ package_name ^ " *)"
      else  
        "(* Auto-generated module aliases for folder " ^ (String.uncapitalize_ascii tree.name) ^ " *)"
    in
    
    let alias_content = 
      comment ^ "\n" ^
      (if direct_child_aliases = [] then
         "(* No module aliases needed for this directory *)\n"
       else
         (direct_child_aliases
          |> List.sort_uniq compare
          |> List.map (fun (simple, namespaced) ->
              Printf.sprintf "module %s = %s" simple namespaced)
          |> String.concat "\n"))
    in
    
    (* Generate alias module files *)
    let alias_ml = alias_module_name ^ ".ml" in
    let alias_cmi = alias_module_name ^ ".cmi" in
    let alias_cmo = alias_module_name ^ ".cmo" in
    
    (* Add alias module actions first (they need to be compiled before modules that open them) *)
    actions := Actions.WriteFile { destination = alias_ml; content = alias_content } :: !actions;
    actions := Actions.CompileInterface 
      { source = alias_ml; output = alias_cmi; includes = dep_includes; flags = [Ocamlc.NoAliasDeps] } :: !actions;
    actions := Actions.CompileImplementation 
      { source = alias_ml; output = alias_cmo; includes = dep_includes; flags = [Ocamlc.NoAliasDeps] } :: !actions;
    outputs := alias_cmi :: alias_cmo :: !outputs;
    
    (* Recursively process subdirectories *)
    List.iter (fun child ->
      if child.children <> [] then
        let child_namespace = 
          if namespace_prefix = "" then
            String.capitalize_ascii safe_package_name ^ "__" ^ String.capitalize_ascii child.name
          else
            namespace_prefix ^ "__" ^ String.capitalize_ascii child.name
        in
        generate_hierarchical_aliases ~tree:child ~package_name ~namespace_prefix:child_namespace ~actions ~outputs ~dep_includes
    ) tree.children
  in
  
  let safe_package_name = String.map (fun c -> if c = '-' then '_' else c) package.Workspace.name in
  let main_alias_name = String.capitalize_ascii safe_package_name ^ "__aliases" in
  
  (* Check if we actually need aliasing at all *)
  (* We need aliasing if we have multiple top-level modules or any subdirectories *)
  let needs_aliasing = 
    (* Check if we have multiple top-level modules or any subdirectories *)
    let top_level_modules = List.filter (fun child -> 
      child.impl <> None || child.intf <> None
    ) tree.children in
    (* Special case: if the package has a single module with the same name as the package, no aliasing needed *)
    if List.length top_level_modules = 1 && List.length tree.children = 1 then
      let single_module = List.hd tree.children in
      (* Check if this single module matches the package name *)
      not (String.lowercase_ascii single_module.name = String.lowercase_ascii package.Workspace.name)
    else
      List.length top_level_modules > 1 || 
      List.exists (fun child -> child.children <> []) tree.children
  in
  
  (* Only generate alias files if we actually need them *)
  let alias_module_name_opt = 
    if needs_aliasing then (
      (* Generate all hierarchical alias files *)
      generate_hierarchical_aliases ~tree ~package_name:package.Workspace.name ~namespace_prefix:"" ~actions ~outputs ~dep_includes;
      Some main_alias_name
    ) else
      None
  in
  
  let alias_cmo_opt = None (* Now handled by generate_actions_from_tree *)
  in
  
  (* Helper to determine the correct open flags for a source file based on its path *)
  let get_open_flags_for_source source_file =
    match alias_module_name_opt with
    | None -> [] (* No alias module, no open flags *)
    | Some main_alias_name ->
        let safe_package_name = String.map (fun c -> if c = '-' then '_' else c) package.Workspace.name in
        let path_str = Std.Path.to_string source_file in
        let src_dir_str = "packages/" ^ package.Workspace.name ^ "/src" in
        
        (* Check if the file is in a subdirectory *)
        if String.starts_with ~prefix:(src_dir_str ^ "/") path_str then
          let relative_path = String.sub path_str (String.length src_dir_str + 1) (String.length path_str - String.length src_dir_str - 1) in
          (* Check if it's in a subdirectory (has a / in the relative path) *)
          match String.index_opt relative_path '/' with
          | Some idx ->
              (* It's in a subdirectory - extract the subdirectory name *)
              let subdir_name = String.sub relative_path 0 idx in
              let subdir_module = String.capitalize_ascii safe_package_name ^ "__" ^ String.capitalize_ascii subdir_name in
              (* Open both the subdirectory wrapper and the main alias module *)
              [Ocamlc.Open (subdir_module ^ "__wrapper"); Ocamlc.Open main_alias_name]
          | None ->
              (* Top-level file - only open the main alias module *)
              [Ocamlc.Open main_alias_name]
        else
          (* Not in src dir - only open the main alias module *)
          [Ocamlc.Open main_alias_name]
  in

  (* Compile .mli files to .cmi with appropriate open flags *)
  List.iter
    (fun mli_source ->
      let mli_str = Std.Path.to_string mli_source.Build_node.file in
      (* Use the pre-computed namespaced module name *)
      let cmi_path = 
        match mli_source.Build_node.kind with
        | Build_node.MLI { namespaced_name; _ } -> namespaced_name ^ ".cmi"
        | _ -> failwith "Internal error: non-MLI source in mli_sources"
      in
      (* Get appropriate open flags based on source location *)
      let open_flags = get_open_flags_for_source mli_source.Build_node.file in
      actions :=
        Actions.CompileInterface
          { source = mli_str; output = cmi_path; includes = dep_includes; flags = open_flags }
        :: !actions;
      outputs := cmi_path :: !outputs)
    sorted_mli_sources;

  (* Compile .c files to .o *)
  let o_files = ref [] in
  List.iter
    (fun c_source ->
      let c_str = Std.Path.to_string c_source.Build_node.file in
      (* C files just use their basename without extension *)
      let basename = Filename.chop_extension (Filename.basename c_str) in
      let o_path = basename ^ ".o" in
      actions :=
        Actions.CompileC { source = c_str; output = o_path } :: !actions;
      o_files := o_path :: !o_files;
      outputs := o_path :: !outputs)
    c_sources;

  (* Compile .ml files to .cmo - DON'T reverse, we're prepending so it reverses naturally *)
  let cmo_files = ref [] in
  (* Add alias module to cmo_files if it exists *)
  (match alias_module_name_opt with
   | Some alias_name -> cmo_files := (alias_name ^ ".cmo") :: !cmo_files
   | None -> ());
  
  List.iter
    (fun ml_source ->
      let ml_str = Std.Path.to_string ml_source.Build_node.file in
      (* Use the pre-computed namespaced module name *)
      let cmo_path = 
        match ml_source.Build_node.kind with
        | Build_node.ML { namespaced_name; _ } -> namespaced_name ^ ".cmo"
        | _ -> failwith "Internal error: non-ML source in ml_sources"
      in
      (* Get appropriate open flags based on source location *)
      let open_flags = get_open_flags_for_source ml_source.Build_node.file in
      actions :=
        Actions.CompileImplementation
          { source = ml_str; output = cmo_path; includes = dep_includes; flags = open_flags }
        :: !actions;
      cmo_files := cmo_path :: !cmo_files;
      outputs := cmo_path :: !outputs)
    sorted_ml_sources;


  (* Always build a library if we have any .ml files *)
  if !cmo_files <> [] then (
    (* Create library *)
    let cma_path = Workspace.(package.name) ^ ".cma" in
    let lib_objects = List.rev !cmo_files @ !o_files in
    actions :=
      Actions.CreateLibrary
        { output = cma_path; objects = lib_objects; includes = dep_includes }
      :: !actions;
    outputs := cma_path :: !outputs);

  (* Check if we should build executables *)
  (* For now, we build an executable if main.ml exists *)
  (* TODO: In the future, use [[bin]] definitions from package config *)
  let has_main_ml =
    List.exists
      (fun s -> Filename.basename (Std.Path.to_string s.Build_node.file) = "main.ml")
      sorted_ml_sources
  in

  if has_main_ml then (
    (* Create executable *)
    let exe_path = Workspace.(package.name) in
    let exe_objects = List.rev !cmo_files @ !o_files in
    (* Include dependency libraries (.cma files) from local packages *)
    (* We need to collect ALL transitive dependencies, not just direct ones *)
    let rec collect_all_deps visited current_node =
      if List.mem current_node.Build_node.package.name visited then []
      else
        let visited = current_node.Build_node.package.name :: visited in
        (* Resolve dependency IDs to nodes *)
        let current_dep_nodes =
          List.map (Build_graph.get_node graph) current_node.Build_node.deps
        in
        let child_deps =
          List.concat_map (collect_all_deps visited) current_dep_nodes
        in
        current_node :: child_deps
    in
    let all_dep_nodes =
      List.concat_map (collect_all_deps []) deps
      |> List.rev (* Reverse to get dependencies before dependents *)
      |> List.fold_left
           (fun acc node ->
             if
               List.exists
                 (fun n ->
                   n.Build_node.package.name = node.Build_node.package.name)
                 acc
             then acc (* Already in list, skip duplicate *)
             else node :: acc)
           [] (* Add unique nodes *)
      |> List.rev (* Reverse back to get correct order: dependencies first *)
    in
    let dep_libs =
      List.map (fun dep -> dep.Build_node.package.name ^ ".cma") all_dep_nodes
    in
    (* Check if we need to add unix.cma transitively *)
    let needs_unix = needs_unix_transitively ~graph ~node in
    let all_external_libs =
      if needs_unix && not (List.mem "unix.cma" external_libs) then
        "unix.cma" :: external_libs
      else external_libs
    in
    let all_libs = all_external_libs @ dep_libs in
    actions :=
      Actions.CreateExecutable
        {
          output = exe_path;
          objects = exe_objects;
          libraries = all_libs;
          includes = dep_includes;
        }
      :: !actions;
    outputs := exe_path :: !outputs);

  (* Add DeclareOutputs action *)
  if !outputs <> [] then
    actions := Actions.DeclareOutputs { outputs = !outputs } :: !actions;

  (* Convert output strings to Path.t for the return value *)
  let output_paths =
    List.filter_map
      (fun s ->
        match Std.Path.of_string s with Ok p -> Some p | Error _ -> None)
      !outputs
  in
  
  (* Return actions in reverse order - alias modules are already added first by generate_actions_from_tree *)
  (List.rev output_paths, List.rev !actions)

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
  List.iter
    (fun dep ->
      match dep.Build_node.spec with
      | Unplanned -> ()
      | Planned { hash; _ } -> seeds := Hasher.to_string hash :: !seeds)
    deps;

  (* Hash actions *)
  let actions_hash = Actions.hash actions in
  seeds := Hasher.to_string actions_hash :: !seeds;

  (* Combine all seeds into final hash *)
  Hasher.hash_strings (List.rev !seeds)

let plan_node ~graph ~node ~build_results ~session_id () =
  (* Step 1: Check if immediate dependencies have been planned/built *)
  (* Resolve dependency IDs to nodes *)
  let dep_nodes = List.map (Build_graph.get_node graph) node.Build_node.deps in

  (* First, check if any dependencies have failed *)
  let failed_deps =
    List.filter_map
      (fun dep ->
        let pkg_name = dep.Build_node.package.name in
        match Build_results.get_status build_results pkg_name with
        | Some (Build_results.Failed error) -> Some (pkg_name, error)
        | _ -> None)
      dep_nodes
  in

  if failed_deps <> [] then
    (* If any dependency has failed, fail this node immediately *)
    (* Just record which dependencies failed, not their full error messages *)
    let dep_errors = List.map (fun (name, _err) -> name) failed_deps in
    Ok (Skipped { node; reason = DependenciesFailed dep_errors })
  else
    (* Check for unplanned/unbuilt dependencies *)
    let unplanned_deps =
      List.filter
        (fun dep ->
          (* A dependency is ready only if it's successfully built *)
          let pkg_name = dep.Build_node.package.name in
          match Build_results.get_status build_results pkg_name with
          | Some (Build_results.Built _) -> false (* Already built - ready *)
          | Some Build_results.Building -> true (* Still building - not ready *)
          | Some Build_results.NotStarted -> true (* Not started - not ready *)
          | Some (Build_results.Failed _) ->
              true (* Should have been caught above *)
          | None -> true (* Not tracked - not ready *))
        dep_nodes
    in

    if unplanned_deps <> [] then
      (* Return MissingDependencies if any deps are unplanned *)
      Ok (MissingDependencies { node; deps = unplanned_deps })
    else
      (* Step 2: All deps are planned, we can proceed *)
      (* Step 3: Generate outputs and actions based on sources, deps, and package *)
      let outs, actions =
        generate_actions ~graph ~node ~toolchain:node.toolchain
          ~package:node.package ~srcs:node.srcs ~deps:dep_nodes
      in

      (* Step 4: Compute SHA512 hash of everything *)
      Log.computing_hash ~session_id ~package:node.package.name;
      let hash_start = time_ms () in
      let hash =
        compute_hash_for_planned_node ~toolchain:node.toolchain
          ~package:node.package ~srcs:node.srcs ~deps:dep_nodes ~outs ~actions
      in
      let hash_duration = time_ms () - hash_start in
      Log.hash_computed ~session_id ~package:node.package.name
        ~hash:(Hasher.to_string hash) ~duration_ms:hash_duration;

      (* Update the node's spec to Planned *)
      node.spec <- Planned { hash; outs; actions };

      Ok (Planned node)
