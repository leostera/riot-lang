type build_output = {
  path: string;
  output_type: [`CMI | `CMO | `CMA | `EXE];
}

type build_blueprint = {
  source_file: Workspace.source_file;
  dependencies: string list; (* Module names this file depends on *)
  outputs: build_output list; (* What this build step will produce *)
}

type build_state = 
  | Pending of build_blueprint
  | Building of build_blueprint
  | Built of build_blueprint * build_output list
  | Failed of string

type build_server = {
  ocaml_version: string;
  build_queue: (string * build_state) list ref; (* module_name -> state *)
  completed_modules: (string * build_output list) list ref; (* module_name -> outputs *)
  package_registry: Package_registry.t;
}

let create_build_server ocaml_version =
  {
    ocaml_version;
    build_queue = ref [];
    completed_modules = ref [];
    package_registry = Package_registry.create ();
  }

let get_ocamldep_path server =
  let home = Sys.getenv "HOME" in
  Printf.sprintf "%s/.tusk/toolchains/%s/bin/ocamldep" home server.ocaml_version

let get_ocamlc_path server =
  let home = Sys.getenv "HOME" in
  Printf.sprintf "%s/.tusk/toolchains/%s/bin/ocamlc" home server.ocaml_version

(* 1. Generate blueprint using ocamldep *)
let generate_blueprint server source_file =
  let ocamldep = get_ocamldep_path server in
  let temp_file = Filename.temp_file "tusk_deps" ".txt" in
  let dep_cmd = Printf.sprintf "%s -modules %s > %s" ocamldep source_file.Workspace.path temp_file in
  
  let dependencies = match Unix.system dep_cmd with
  | Unix.WEXITED 0 ->
      let ic = open_in temp_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Sys.remove temp_file;
      
      (* Parse "file.ml: Module1 Module2 Module3" format *)
      let parts = String.split_on_char ':' content in
      if List.length parts >= 2 then
        let deps_str = String.trim (List.nth parts 1) in
        if deps_str = "" then []
        else String.split_on_char ' ' deps_str 
             |> List.map String.trim 
             |> List.filter (fun s -> s <> "")
      else []
  | _ ->
      Printf.printf "Warning: ocamldep failed for %s\n%!" source_file.Workspace.path;
      (try Sys.remove temp_file with _ -> ());
      []
  in
  
  let outputs = match source_file.Workspace.file_type with
    | `MLI -> [{ path = source_file.Workspace.module_name ^ ".cmi"; output_type = `CMI }]
    | `ML -> [
        { path = source_file.Workspace.module_name ^ ".cmi"; output_type = `CMI };
        { path = source_file.Workspace.module_name ^ ".cmo"; output_type = `CMO };
      ]
  in
  
  { source_file; dependencies; outputs }

(* 2. Check if all modules needed by file have been built *)
let dependencies_ready server blueprint =
  List.for_all (fun module_name ->
    List.assoc_opt module_name !(server.completed_modules) |> Option.is_some
  ) blueprint.dependencies

(* 3. Get transitive graph of dependencies: current_file.deps |> flat_map outputs |> concat *)
let get_transitive_outputs server module_name =
  let rec collect_outputs visited module_name =
    if List.mem module_name visited then []
    else
      match List.assoc_opt module_name !(server.completed_modules) with
      | Some outputs ->
          (* Get this module's outputs *)
          let direct_outputs = outputs in
          (* Get transitive outputs from dependencies *)
          let transitive_outputs = 
            (* Find the blueprint for this module to get its dependencies *)
            match List.assoc_opt module_name !(server.build_queue) with
            | Some (Built (blueprint, _)) ->
                List.fold_left (fun acc dep ->
                  let dep_outputs = collect_outputs (module_name :: visited) dep in
                  dep_outputs @ acc
                ) [] blueprint.dependencies
            | _ -> []
          in
          direct_outputs @ transitive_outputs
      | None -> []
  in
  collect_outputs [] module_name

(* Content-Addressed Storage - following Ox architecture *)
module ContentStore = struct
  type manifest = {
    outputs: build_output list;
    build_time: float;
    toolchain_version: string;
  }
  
  let cache_dir () =
    let home = Sys.getenv "HOME" in
    Filename.concat home ".tusk/cache"
  
  let ensure_cache_dir () =
    let dir = cache_dir () in
    let rec mkdir_p path =
      if not (Sys.file_exists path) then (
        mkdir_p (Filename.dirname path);
        Unix.mkdir path 0o755
      )
    in
    mkdir_p dir
  
  let hash_inputs ~source_file ~dependencies ~toolchain_version =
    let inputs = [
      "tusk_build_v1"; (* Build system version *)
      source_file.Workspace.path;
      source_file.Workspace.package;
      source_file.Workspace.module_name;
      (match source_file.Workspace.file_type with `ML -> "ml" | `MLI -> "mli");
      String.concat ";" dependencies;
      toolchain_version;
    ] in
    let combined = String.concat "|" inputs in
    let hash_cmd = Printf.sprintf "echo '%s' | shasum -a 256 | cut -d' ' -f1" combined in
    let ic = Unix.open_process_in hash_cmd in
    let hash = input_line ic in
    ignore (Unix.close_process_in ic);
    String.trim hash
  
  let get_cached_result hash =
    ensure_cache_dir ();
    let manifest_path = Filename.concat (cache_dir ()) (hash ^ ".manifest") in
    if Sys.file_exists manifest_path then
      try
        let ic = open_in manifest_path in
        let _content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        (* Simple JSON-like parsing for manifest *)
        Some { 
          outputs = []; (* TODO: Parse from manifest *)
          build_time = Unix.time ();
          toolchain_version = "";
        }
      with _ -> None
    else None
  
  let store_result hash manifest outputs_dir =
    ensure_cache_dir ();
    let cache_base = cache_dir () in
    let manifest_path = Filename.concat cache_base (hash ^ ".manifest") in
    let outputs_path = Filename.concat cache_base (hash ^ ".outputs") in
    
    (* Store manifest *)
    let oc = open_out manifest_path in
    Printf.fprintf oc "{\n  \"build_time\": %f,\n  \"toolchain\": \"%s\"\n}\n" 
      manifest.build_time manifest.toolchain_version;
    close_out oc;
    
    (* Copy outputs to cache *)
    if not (Sys.file_exists outputs_path) then Unix.mkdir outputs_path 0o755;
    List.iter (fun output ->
      if Sys.file_exists output.path then (
        let target_path = Filename.concat outputs_path (Filename.basename output.path) in
        let copy_cmd = Printf.sprintf "cp %s %s" output.path target_path in
        ignore (Unix.system copy_cmd)
      )
    ) manifest.outputs
end

let execute_build_step server blueprint =
  let module_name = blueprint.source_file.Workspace.module_name in
  
  if not (dependencies_ready server blueprint) then (
    Printf.printf "%s dependencies not ready, requeuing...\n%!" module_name;
    false
  ) else (
    (* Content-addressed caching check *)
    let content_hash = ContentStore.hash_inputs 
      ~source_file:blueprint.source_file 
      ~dependencies:blueprint.dependencies 
      ~toolchain_version:server.ocaml_version in
    
    match ContentStore.get_cached_result content_hash with
    | Some cached_manifest ->
        Printf.printf "✓ %s (cached)\n%!" module_name;
        (* Record cached completion *)
        server.completed_modules := (module_name, blueprint.outputs) :: !(server.completed_modules);
        server.build_queue := List.map (fun (name, state) ->
          if name = module_name then (name, Built (blueprint, blueprint.outputs))
          else (name, state)
        ) !(server.build_queue);
        true
    | None ->
        Printf.printf "Building %s...\n%!" module_name;
        
        (* Create sandbox with transitive dependencies *)
        let transitive_deps = List.fold_left (fun acc dep_module ->
          let dep_outputs = get_transitive_outputs server dep_module in
          dep_outputs @ acc
        ) [] blueprint.dependencies in
        
        let sandbox_dir = Printf.sprintf "./target/sandbox/%s" content_hash in
    
    (* Create sandbox *)
    (try Unix.mkdir (Filename.dirname sandbox_dir) 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    (try Unix.mkdir sandbox_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    
    (* Copy source file *)
    let src_basename = Filename.basename blueprint.source_file.Workspace.path in
    let copy_src_cmd = Printf.sprintf "cp %s %s/" blueprint.source_file.Workspace.path sandbox_dir in
    ignore (Unix.system copy_src_cmd);
    
    (* Copy transitive dependencies *)
    List.iter (fun output ->
      if Sys.file_exists output.path then (
        let copy_cmd = Printf.sprintf "cp %s %s/" output.path sandbox_dir in
        ignore (Unix.system copy_cmd)
      )
    ) transitive_deps;
    
    (* Execute build *)
    let ocamlc = get_ocamlc_path server in
    let build_cmd = match blueprint.source_file.Workspace.file_type with
      | `MLI -> Printf.sprintf "cd %s && %s -c %s" sandbox_dir ocamlc src_basename
      | `ML -> Printf.sprintf "cd %s && %s -c %s" sandbox_dir ocamlc src_basename
    in
    
        match Unix.system build_cmd with
        | Unix.WEXITED 0 ->
            (* Move outputs to final location *)
            let actual_outputs = List.map (fun output ->
              let sandbox_output = Filename.concat sandbox_dir output.path in
              let final_output = output.path in
              if Sys.file_exists sandbox_output then (
                let move_cmd = Printf.sprintf "cp %s %s" sandbox_output final_output in
                ignore (Unix.system move_cmd)
              );
              output
            ) blueprint.outputs in
            
            (* Store in content-addressed cache *)
            let manifest = ContentStore.{
              outputs = actual_outputs;
              build_time = Unix.time ();
              toolchain_version = server.ocaml_version;
            } in
            ContentStore.store_result content_hash manifest sandbox_dir;
            
            (* Record completion *)
            server.completed_modules := (module_name, actual_outputs) :: !(server.completed_modules);
            server.build_queue := List.map (fun (name, state) ->
              if name = module_name then (name, Built (blueprint, actual_outputs))
              else (name, state)
            ) !(server.build_queue);
            
            Printf.printf "✓ Built %s\n%!" module_name;
            true
        | _ ->
            Printf.printf "✗ Failed to build %s\n%!" module_name;
            server.build_queue := List.map (fun (name, state) ->
              if name = module_name then (name, Failed "Build command failed")
              else (name, state)
            ) !(server.build_queue);
            false
  )

let receive_files server files =
  Printf.printf "Received %d files for consideration\n%!" (List.length files);
  
  (* Generate blueprints and add to queue *)
  List.iter (fun source_file ->
    let blueprint = generate_blueprint server source_file in
    let module_name = source_file.Workspace.module_name in
    server.build_queue := (module_name, Pending blueprint) :: !(server.build_queue);
    Printf.printf "Queued %s (deps: [%s])\n%!" 
      module_name (String.concat "; " blueprint.dependencies)
  ) files;
  
  (* Process build queue until no more progress *)
  let rec process_until_no_progress () =
    let initial_completed = List.length !(server.completed_modules) in
    
    (* Try to build anything that's ready *)
    server.build_queue := List.map (fun (name, state) ->
      match state with
      | Pending blueprint ->
          if execute_build_step server blueprint then
            (name, state) (* Will be updated by execute_build_step *)
          else
            (name, Pending blueprint)
      | other -> (name, other)
    ) !(server.build_queue);
    
    let final_completed = List.length !(server.completed_modules) in
    if final_completed > initial_completed then
      process_until_no_progress () (* Made progress, try again *)
    else
      Printf.printf "Build queue processing complete: %d/%d modules built\n%!" 
        final_completed (List.length !(server.build_queue))
  in
  process_until_no_progress ()