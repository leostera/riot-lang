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
        let path_key = String.concat "/" namespace in
        let existing = Hashtbl.find_opt path_map path_key |> Option.value ~default:(simple_name, None, None) in
        let (name, _, intf) = existing in
        Hashtbl.replace path_map path_key (name, Some source, intf)
    | Build_node.MLI { simple_name; namespace; _ } ->
        let path_key = String.concat "/" namespace in
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
        if path = "" then
          (* This is a root module *)
          { name; impl; intf; 
            root = Path.of_string "." |> Result.expect ~msg:"Invalid path '.'"; 
            children = [] } :: acc
        else if not (String.contains path '/') then
          (* This is a folder at root level *)
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
      (* Collect aliases for modules at this level *)
      let module_aliases = 
        List.filter_map (fun child ->
          match child.impl, child.intf with
          | Some impl, _ ->
              (match impl.Build_node.kind with
               | Build_node.ML { simple_name; namespaced_name; _ } ->
                   (* Include all modules at package level (level=0), but exclude folder interfaces at deeper levels *)
                   if level = 0 || simple_name <> tree.name then
                     Some (simple_name, namespaced_name)
                   else None
               | _ -> None)
          | None, Some intf ->
              (match intf.Build_node.kind with
               | Build_node.MLI { simple_name; namespaced_name; _ } ->
                   (* Include all modules at package level (level=0), but exclude folder interfaces at deeper levels *)
                   if level = 0 || simple_name <> tree.name then
                     Some (simple_name, namespaced_name)
                   else None
               | _ -> None)
          | None, None when child.children <> [] ->
              (* This is a subfolder *)
              let folder_name = String.capitalize_ascii child.name in
              let namespaced_folder = 
                if level = 0 then
                  String.capitalize_ascii safe_package_name ^ "__" ^ folder_name
                else
                  let parent_namespace = 
                    match parent_aliases with
                    | Some (parent_name, _) -> parent_name ^ "__"
                    | None -> String.capitalize_ascii safe_package_name ^ "__"
                  in
                  parent_namespace ^ folder_name
              in
              Some (folder_name, namespaced_folder)
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

  (* Prepare alias module configuration but don't add actions yet *)
  let alias_module_name_opt, open_flags, alias_cmo_opt = 
    (* Build module tree and collect ALL modules that need aliases *)
    let tree = build_module_tree ~package_name:package.Workspace.name ~srcs in
    
    
    (* Recursively collect all modules from the tree *)
    let rec collect_all_modules tree =
      let current_modules = 
        (* Add module for current tree node if it has implementation or interface *)
        (match tree.impl with
         | Some impl -> 
             (match impl.Build_node.kind with
              | Build_node.ML { simple_name; namespaced_name; _ } ->
                  if simple_name = namespaced_name then [] else [(simple_name, namespaced_name)]
              | _ -> [])
         | None -> []) @
        (match tree.intf with
         | Some intf -> 
             (match intf.Build_node.kind with
              | Build_node.MLI { simple_name; namespaced_name; _ } ->
                  (* Only add MLI if there's no corresponding ML *)
                  if tree.impl = None && simple_name <> namespaced_name then [(simple_name, namespaced_name)] else []
              | _ -> [])
         | None -> [])
      in
      (* Add folder aliases for children that have children (subfolders) *)
      let folder_modules = 
        List.filter_map (fun child ->
          if child.children <> [] then
            (* This is a subfolder, create an alias for it *)
            let folder_name = String.capitalize_ascii child.name in
            let safe_package_name = String.map (fun c -> if c = '-' then '_' else c) package.Workspace.name in
            let namespaced_folder = String.capitalize_ascii safe_package_name ^ "__" ^ folder_name in
            Some (folder_name, namespaced_folder)
          else None
        ) tree.children
      in
      (* Recursively collect from children *)
      let child_modules = List.concat_map collect_all_modules tree.children in
      current_modules @ folder_modules @ child_modules
    in
    
    let module_aliases = collect_all_modules tree |> List.sort_uniq compare in
    
    if module_aliases <> [] then (
      (* Generate alias module content *)
      let alias_content = 
        "(* Auto-generated module aliases for package " ^ package.Workspace.name ^ " *)\n" ^
        (module_aliases
         |> List.map (fun (simple, namespaced) ->
             Printf.sprintf "module %s = %s" simple namespaced)
         |> String.concat "\n")
      in
      
      
      (* Create a unique alias module name *)
      let safe_name = String.map (fun c -> if c = '-' then '_' else c) package.Workspace.name in
      let alias_module_name = String.capitalize_ascii safe_name ^ "__aliases" in
      let alias_ml = alias_module_name ^ ".ml" in
      let alias_cmo = alias_module_name ^ ".cmo" in
      
      (Some alias_module_name, [Ocamlc.Open alias_module_name], Some (alias_ml, alias_cmo, alias_content))
    ) else (None, [], None)
  in

  (* Compile .mli files to .cmi with -open flag if we have aliases *)
  List.iter
    (fun mli_source ->
      let mli_str = Std.Path.to_string mli_source.Build_node.file in
      (* Use the pre-computed namespaced module name *)
      let cmi_path = 
        match mli_source.Build_node.kind with
        | Build_node.MLI { namespaced_name; _ } -> namespaced_name ^ ".cmi"
        | _ -> failwith "Internal error: non-MLI source in mli_sources"
      in
      (* Add -open flag if we have an alias module *)
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
      (* Add -open flag if we have an alias module *)
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

  (* Add alias module to outputs BEFORE DeclareOutputs *)
  (match alias_cmo_opt with
  | Some (_, alias_cmo, _) ->
      (* Add both .cmo and .cmi files for the alias module *)
      let alias_cmi = Filename.chop_extension alias_cmo ^ ".cmi" in
      outputs := alias_cmi :: alias_cmo :: !outputs
  | None -> ());

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
  
  (* Build final action list with alias module FIRST *)
  let final_actions = 
    match alias_cmo_opt with
    | Some (alias_ml, alias_cmo, alias_content) ->
        (* Alias module actions go first *)
        let alias_actions = [
          Actions.WriteFile { destination = alias_ml; content = alias_content };
          Actions.CompileImplementation 
            { source = alias_ml; output = alias_cmo; 
              includes = dep_includes;
              flags = [Ocamlc.NoAliasDeps] }
        ] in
        (* Alias actions first, then all other actions (reversed) *)
        alias_actions @ (List.rev !actions)
    | None -> 
        List.rev !actions
  in
  (List.rev output_paths, final_actions)

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
