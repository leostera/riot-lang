(** Minitusk CLI - minitusk build *)


(** Minitusk build system *)
module Minitusk = struct
  type profile = Debug

  module Config = struct
    type t = {
      profile: profile;
      ocaml_version: string;
    }
    
    let default = {
      profile = Debug;
      ocaml_version = "5.3.0";
    }
    
    let pp fmt t =
      Format.fprintf fmt "Config(profile=%s, ocaml_version=%s)"
        (match t.profile with Debug -> "debug")
        t.ocaml_version
  end

  module Toolchain = struct
    let tusk_dir () = 
      let home = Sys.getenv "HOME" in
      Filename.concat home ".tusk"
    
    let cache_dir () =
      let tusk = tusk_dir () in
      Filename.concat tusk "cache"
    
    let toolchain_dir version =
      let tusk = tusk_dir () in
      Filename.concat (Filename.concat tusk "toolchains") version
    
    let ocamlc_path version =
      let toolchain = toolchain_dir version in
      Filename.concat (Filename.concat toolchain "bin") "ocamlc"
    
    let ocamldep_path version =
      let toolchain = toolchain_dir version in
      Filename.concat (Filename.concat toolchain "bin") "ocamldep"
    
    let ensure_dirs () =
      let tusk = tusk_dir () in
      (try Unix.mkdir tusk 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      let toolchains = Filename.concat tusk "toolchains" in
      (try Unix.mkdir toolchains 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      let cache = cache_dir () in
      (try Unix.mkdir cache 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    
    let toolchain_exists version =
      let ocamlc = ocamlc_path version in
      Sys.file_exists ocamlc
    
    let url_to_cache_path url =
      let cache = cache_dir () in
      (* Convert URL to filesystem path, preserving protocol *)
      let path = String.map (function ':' -> '/' | c -> c) url in
      Filename.concat cache path
    
    let download_if_needed url =
      let cache_path = url_to_cache_path url in
      if Sys.file_exists cache_path then (
        Printf.printf "Using cached download: %s\n%!" cache_path;
        cache_path
      ) else (
        Printf.printf "Downloading: %s\n%!" url;
        (* Ensure parent directory exists *)
        let parent_dir = Filename.dirname cache_path in
        let rec mkdir_p dir =
          if not (Sys.file_exists dir) then (
            mkdir_p (Filename.dirname dir);
            try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
          ) in
        mkdir_p parent_dir;
        
        let download_cmd = Printf.sprintf "curl -L -o %s %s" cache_path url in
        match Unix.system download_cmd with
        | Unix.WEXITED 0 -> 
            Printf.printf "Downloaded to cache: %s\n%!" cache_path;
            cache_path
        | _ -> failwith ("Failed to download " ^ url)
      )
    
    let download_and_build_ocaml version =
      Printf.printf "Setting up OCaml %s toolchain...\n%!" version;
      let toolchain = toolchain_dir version in
      
      (* Create toolchain directory *)
      (try Unix.mkdir toolchain 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      
      (* Download OCaml source to cache *)
      let url = Printf.sprintf "https://github.com/ocaml/ocaml/archive/%s.tar.gz" version in
      let tarball = download_if_needed url in
      
      (* Extract to toolchain directory *)
      Printf.printf "Extracting OCaml source...\n%!";
      let extract_cmd = Printf.sprintf "cd %s && tar -xzf %s --strip-components=1" toolchain tarball in
      (match Unix.system extract_cmd with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith ("Failed to extract OCaml " ^ version));
      
      (* Configure *)
      Printf.printf "Configuring OCaml build...\n%!";
      let configure_cmd = Printf.sprintf "cd %s && ./configure --prefix=%s" toolchain toolchain in
      (match Unix.system configure_cmd with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith ("Failed to configure OCaml " ^ version));
      
      (* Build *)
      Printf.printf "Building OCaml (this may take several minutes)...\n%!";
      let num_cores = Domain.recommended_domain_count () in
      let build_cmd = Printf.sprintf "cd %s && make -j%d world.opt" toolchain num_cores in
      (match Unix.system build_cmd with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith ("Failed to build OCaml " ^ version));
      
      (* Install *)
      Printf.printf "Installing OCaml to toolchain...\n%!";
      let install_cmd = Printf.sprintf "cd %s && make install" toolchain in
      (match Unix.system install_cmd with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith ("Failed to install OCaml " ^ version));
      
      Printf.printf "✓ OCaml %s toolchain ready!\n%!" version
    
    (* Global toolchain cache to avoid multiple downloads *)
    let _toolchain_cache = ref None
    
    let ensure_toolchain version =
      match !_toolchain_cache with
      | Some cached_version when cached_version = version -> 
          (* Already ensured this version *)
          ()
      | _ ->
          ensure_dirs ();
          if not (toolchain_exists version) then (
            download_and_build_ocaml version;
            _toolchain_cache := Some version
          ) else (
            Printf.printf "✓ OCaml %s toolchain already available\n%!" version;
            _toolchain_cache := Some version
          )
    
    let get_ocamlc_path version =
      ensure_toolchain version;
      ocamlc_path version
  end

  module Package = struct
    type t = {
      name: string;
      path: string;
      sources: string list;
      executables: string list;
      dependencies: string list;
    }
    
    let hardcoded_packages root_dir = [
      {
        name = "gluon";
        path = Filename.concat root_dir "packages/gluon";
        sources = []; (* Will be populated dynamically when needed *)
        executables = [];
        dependencies = [];
      };
      {
        name = "miniriot";
        path = Filename.concat root_dir "packages/miniriot";
        sources = []; (* Will be populated dynamically when needed *)
        executables = [];
        dependencies = [];
      };
      {
        name = "minitusk";
        path = Filename.concat root_dir "packages/minitusk";
        sources = []; (* Will be populated dynamically when needed *)
        executables = [Filename.concat root_dir "packages/minitusk/main.ml"];
        dependencies = [];
      };
      {
        name = "tusk";
        path = Filename.concat root_dir "packages/tusk";
        sources = []; (* Will be populated dynamically when needed *)
        executables = [Filename.concat root_dir "packages/tusk/main.ml"];
        dependencies = [];
      };
    ]
    
    let find_files dir pattern =
      try
        let files = Sys.readdir dir in
        Array.to_list files
        |> List.filter (fun f -> 
            let len = String.length pattern in
            String.length f >= len && 
            String.sub f (String.length f - len) len = pattern)
        |> List.map (fun f -> Filename.concat dir f)
      with _ -> []
    
    let populate_sources package =
      let ml_files = find_files package.path ".ml" in
      let mli_files = find_files package.path ".mli" in
      let sources = List.sort_uniq String.compare (ml_files @ mli_files) in
      { package with sources }
    
    let get_packages root_dir =
      List.map populate_sources (hardcoded_packages root_dir)
    
    let pp fmt t =
      Format.fprintf fmt "Package(%s: %d sources, %d executables)"
        t.name (List.length t.sources) (List.length t.executables)
  end

  let execute_command cmd =
    Printf.printf "Executing: %s\n%!" cmd;
    match Unix.system cmd with
    | Unix.WEXITED 0 -> Ok ()
    | Unix.WEXITED code -> Error (Printf.sprintf "Command failed with exit code %d: %s" code cmd)
    | Unix.WSIGNALED _ -> Error (Printf.sprintf "Command killed by signal: %s" cmd)
    | Unix.WSTOPPED _ -> Error (Printf.sprintf "Command stopped: %s" cmd)

  let hash_string s =
    (* Simple SHA256 implementation using system command *)
    let temp_file = Filename.temp_file "minitusk_hash" ".txt" in
    let oc = open_out temp_file in
    output_string oc s;
    close_out oc;
    let hash_cmd = Printf.sprintf "shasum -a 256 %s | cut -d' ' -f1" temp_file in
    let ic = Unix.open_process_in hash_cmd in
    let hash = input_line ic in
    ignore (Unix.close_process_in ic);
    Sys.remove temp_file;
    String.trim hash

  let compute_build_hash package_name sources ocaml_version =
    let content_hashes = List.map (fun file ->
      try
        let ic = open_in file in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        file ^ ":" ^ hash_string content
      with _ -> file ^ ":missing"
    ) sources in
    let combined = String.concat "|" (package_name :: ocaml_version :: content_hashes) in
    hash_string combined

  let get_sandbox_dir target_dir build_hash =
    let sandbox_root = Filename.concat target_dir "sandbox" in
    let sandbox_dir = Filename.concat sandbox_root build_hash in
    (* Ensure sandbox directories exist *)
    (try Unix.mkdir sandbox_root 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    (try Unix.mkdir sandbox_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    sandbox_dir

  type build_node = {
    id: string;
    inputs: string list;
    outputs: string list;
    command: string;
    dependencies: string list; (* IDs of other nodes this depends on *)
  }

  type build_graph = (string, build_node) Hashtbl.t

  let create_build_graph () = Hashtbl.create 16

  let add_node graph node = 
    Hashtbl.replace graph node.id node

  let get_transitive_dependencies graph node_id =
    let visited = Hashtbl.create 16 in
    let rec collect_deps id =
      if Hashtbl.mem visited id then []
      else (
        Hashtbl.add visited id ();
        let node = Hashtbl.find graph id in
        let dep_files = List.fold_left (fun acc dep_id ->
          let dep_node = Hashtbl.find graph dep_id in
          let transitive = collect_deps dep_id in
          dep_node.outputs @ transitive @ acc
        ) [] node.dependencies in
        dep_files
      )
    in
    collect_deps node_id

  let execute_node target_dir graph sandbox_cache node_id =
    let node = Hashtbl.find graph node_id in
    let transitive_files = get_transitive_dependencies graph node_id in
    let all_inputs = node.inputs @ transitive_files in
    Printf.printf "    Dependencies: [%s] -> Files: [%s]\n%!" 
      (String.concat "; " node.dependencies)
      (String.concat "; " transitive_files);
    
    (* Create step hash from node and all its dependencies *)
    let dep_contents = List.fold_left (fun acc file ->
      try
        let ic = open_in file in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        acc ^ file ^ ":" ^ content
      with _ -> acc ^ file ^ ":missing"
    ) "" all_inputs in
    let node_hash = hash_string (node.id ^ "|" ^ node.command ^ "|" ^ dep_contents) in
    let sandbox_dir = get_sandbox_dir target_dir node_hash in
    
    Printf.printf "Node: %s (sandbox: %s)\n%!" node.id node_hash;
    
    (* Check if outputs already exist *)
    let outputs_exist = List.for_all (fun out ->
      let out_path = Filename.concat sandbox_dir (Filename.basename out) in
      Sys.file_exists out_path
    ) node.outputs in
    
    if outputs_exist then (
      Printf.printf "  Outputs cached, skipping\n%!";
      Ok sandbox_dir
    ) else (
      (* Copy all input files to sandbox *)
      List.iter (fun input ->
        let basename = Filename.basename input in
        let dest = Filename.concat sandbox_dir basename in
        if Sys.file_exists input then (
          let copy_cmd = Printf.sprintf "cp %s %s" input dest in
          ignore (Unix.system copy_cmd)
        ) else (
          (* Try to find in dependency sandboxes *)
          let found = List.fold_left (fun found dep_id ->
            if found then found else (
              match Hashtbl.find_opt sandbox_cache dep_id with
              | Some dep_sandbox ->
                  let src = Filename.concat dep_sandbox basename in
                  if Sys.file_exists src then (
                    let copy_cmd = Printf.sprintf "cp %s %s" src dest in
                    ignore (Unix.system copy_cmd);
                    true
                  ) else false
              | None -> false
            )
          ) false node.dependencies in
          if not found then
            Printf.printf "  Warning: Input %s not found\n%!" input
        )
      ) all_inputs;
      
      (* Execute command in sandbox *)
      let cmd_in_sandbox = Printf.sprintf "cd %s && %s" sandbox_dir node.command in
      match execute_command cmd_in_sandbox with
      | Error _ as err -> err
      | Ok () ->
          let missing_outputs = List.filter (fun out ->
            let out_path = Filename.concat sandbox_dir (Filename.basename out) in
            not (Sys.file_exists out_path)
          ) node.outputs in
          
          if missing_outputs = [] then (
            Printf.printf "  ✓ All outputs created\n%!";
            Ok sandbox_dir
          ) else (
            Error (Printf.sprintf "Missing expected outputs: %s" (String.concat ", " missing_outputs))
          )
    )


  let get_file_dependencies ocamldep_path ml_file =
    (* Get the dependencies of a single ML file *)
    let temp_file = Filename.temp_file "minitusk_deps" ".txt" in
    let dep_cmd = Printf.sprintf "%s -modules %s > %s" ocamldep_path ml_file temp_file in
    
    match Unix.system dep_cmd with
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
          else String.split_on_char ' ' deps_str |> List.map String.trim |> List.filter (fun s -> s <> "")
        else []
    | _ ->
        Printf.printf "Warning: ocamldep failed for %s\n%!" ml_file;
        (try Sys.remove temp_file with _ -> ());
        []

  let module_name_to_file sources module_name =
    (* Convert "Message" -> "message.ml" *)
    let lowercase_name = String.lowercase_ascii module_name in
    List.find_opt (fun f ->
      let basename = Filename.basename f in
      let name_part = if String.ends_with ~suffix:".ml" basename then
        Filename.chop_suffix basename ".ml"
      else if String.ends_with ~suffix:".mli" basename then
        Filename.chop_suffix basename ".mli"
      else basename in
      String.lowercase_ascii name_part = lowercase_name
    ) sources

  let build_package target_dir _profile config package =
    (* Get OCaml tools from toolchain (already ensured to exist) *)
    let ocamlc = Toolchain.ocamlc_path config.Config.ocaml_version in
    let ocamldep = Toolchain.ocamldep_path config.Config.ocaml_version in
    
    (* Ensure target directory exists *)
    (try Unix.mkdir target_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    
    Printf.printf "Building package: %s\n%!" package.Package.name;
    
    (* Build library if there are non-main sources *)
    let non_main_sources = List.filter (fun f -> 
      String.ends_with ~suffix:".ml" f &&
      not (String.equal (Filename.basename f) "main.ml")) package.sources in
    
    let mli_sources = List.filter (fun f -> String.ends_with ~suffix:".mli" f) package.sources in
    let all_lib_sources = non_main_sources @ mli_sources in
    
    let lib_result = 
      if all_lib_sources = [] then Ok ()
      else (
        (* Create one compilation node that includes all necessary files *)
        let lib_hash = compute_build_hash ("library_" ^ package.Package.name) all_lib_sources config.Config.ocaml_version in
        let sandbox_dir = get_sandbox_dir target_dir lib_hash in
        
        Printf.printf "Library compilation (sandbox: %s)\n%!" lib_hash;
        
        (* Check if library outputs already exist *)
        let cma_path = Filename.concat sandbox_dir (package.Package.name ^ ".cma") in
        if Sys.file_exists cma_path then (
          Printf.printf "  Library cached, skipping\n%!";
          Ok ()
        ) else (
          (* Copy all source files that might be needed *)
          Printf.printf "  Analyzing dependencies with ocamldep...\n%!";
          let all_needed_files = ref (List.sort_uniq String.compare all_lib_sources) in
          
          (* Use ocamldep to find additional dependencies *)
          List.iter (fun ml_file ->
            let deps = get_file_dependencies ocamldep ml_file in
            List.iter (fun module_name ->
              match module_name_to_file package.sources module_name with
              | Some dep_file -> 
                  all_needed_files := dep_file :: !all_needed_files
              | None -> 
                  Printf.printf "    Warning: Could not find source for module %s\n%!" module_name
            ) deps
          ) non_main_sources;
          
          let final_files = List.sort_uniq String.compare !all_needed_files in
          Printf.printf "  Copying files: %s\n%!" (String.concat " " (List.map Filename.basename final_files));
          
          (* Copy all files to sandbox *)
          List.iter (fun file ->
            let basename = Filename.basename file in
            let dest = Filename.concat sandbox_dir basename in
            let copy_cmd = Printf.sprintf "cp %s %s" file dest in
            ignore (Unix.system copy_cmd)
          ) final_files;
          
          (* Use ocamldep to sort ML files in sandbox *)
          let ml_files_in_sandbox = List.filter (fun f -> String.ends_with ~suffix:".ml" f) 
            (List.map (fun f -> Filename.concat sandbox_dir (Filename.basename f)) final_files) in
          let non_main_in_sandbox = List.filter (fun f -> 
            not (String.equal (Filename.basename f) "main.ml")) ml_files_in_sandbox in
          
          if non_main_in_sandbox <> [] then (
            let sort_cmd = Printf.sprintf "cd %s && %s -sort %s"
              sandbox_dir ocamldep (String.concat " " (List.map Filename.basename non_main_in_sandbox)) in
            
            let temp_file = Filename.temp_file "minitusk_sorted" ".txt" in
            let full_sort_cmd = Printf.sprintf "%s > %s" sort_cmd temp_file in
            
            match Unix.system full_sort_cmd with
            | Unix.WEXITED 0 ->
                let ic = open_in temp_file in
                let content = really_input_string ic (in_channel_length ic) in
                close_in ic;
                Sys.remove temp_file;
                
                let sorted_basenames = String.split_on_char ' ' content
                  |> List.map String.trim
                  |> List.filter (fun s -> s <> "" && String.ends_with ~suffix:".ml" s) in
                
                Printf.printf "  Sorted compilation order: %s\n%!" (String.concat " " sorted_basenames);
                
                (* First compile .mli files *)
                let mli_files_in_sandbox = List.filter (fun f -> String.ends_with ~suffix:".mli" f) final_files in
                let mli_basenames = List.map Filename.basename mli_files_in_sandbox in
                
                let mli_result = if mli_basenames = [] then Ok () else (
                  let mli_cmd = Printf.sprintf "cd %s && %s -c %s" 
                    sandbox_dir ocamlc (String.concat " " mli_basenames) in
                  execute_command mli_cmd
                ) in
                
                match mli_result with
                | Error _ as err -> err
                | Ok () ->
                    (* Then compile all .ml files *)
                    let compile_cmd = Printf.sprintf "cd %s && %s -a -o %s.cma %s" 
                      sandbox_dir ocamlc package.Package.name (String.concat " " sorted_basenames) in
                    execute_command compile_cmd
            | _ ->
                Sys.remove temp_file;
                Error "Failed to sort files with ocamldep"
          ) else (
            Ok ()
          )
        )
      ) in
    
    (* Build executables *)
    let exe_result = 
      List.fold_left (fun acc main_file ->
        match acc with 
        | Error _ as err -> err
        | Ok () ->
            let exe_hash = compute_build_hash ("executable_" ^ package.Package.name) [main_file] config.Config.ocaml_version in
            let sandbox_dir = get_sandbox_dir target_dir exe_hash in
            
            Printf.printf "Executable compilation (sandbox: %s)\n%!" exe_hash;
            
            let exe_path = Filename.concat sandbox_dir package.Package.name in
            if Sys.file_exists exe_path then (
              Printf.printf "  Executable cached, skipping\n%!";
              Ok ()
            ) else (
              (* Copy main file to sandbox *)
              let basename = Filename.basename main_file in
              let dest = Filename.concat sandbox_dir basename in
              let copy_cmd = Printf.sprintf "cp %s %s" main_file dest in
              ignore (Unix.system copy_cmd);
              
              (* Compile executable *)
              let compile_cmd = Printf.sprintf "cd %s && %s -I +unix unix.cma -o %s %s" 
                sandbox_dir ocamlc package.Package.name basename in
              match execute_command compile_cmd with
              | Error _ as err -> err
              | Ok () ->
                  (* Copy to final location *)
                  let exe_final = Filename.concat target_dir package.Package.name in
                  let final_copy_cmd = Printf.sprintf "cp %s %s" exe_path exe_final in
                  execute_command final_copy_cmd
            )
      ) (Ok ()) package.executables in
    
    match lib_result, exe_result with
    | Ok (), Ok () -> Ok ()
    | Error msg, _ -> Error msg
    | _, Error msg -> Error msg

  let build ~root_dir ~config =
    Printf.printf "Building in %s\n%!" root_dir;
    
    (* Ensure toolchain is available first *)
    Printf.printf "Ensuring OCaml %s toolchain...\n%!" config.Config.ocaml_version;
    Toolchain.ensure_toolchain config.Config.ocaml_version;
    
    (* Get hardcoded packages *)
    let packages = Package.get_packages root_dir in
    Printf.printf "Discovered %d packages\n%!" (List.length packages);
    List.iter (fun pkg -> Printf.printf "  - %s\n%!" pkg.Package.name) packages;
    
    if packages = [] then (
      Printf.printf "No packages found\n%!";
      Ok ()
    ) else (
      let target_dir = Filename.concat root_dir "target" in
      
      (* Build each package *)
      List.fold_left (fun acc package ->
        match acc with
        | Error _ as err -> err
        | Ok () -> build_package target_dir config.Config.profile config package
      ) (Ok ()) packages
    )
end

let usage_msg = "minitusk build"

let build_command () =
  let root_dir = Sys.getcwd () in
  let config = Minitusk.Config.default in
  
  Format.printf "Configuration: %a\n%!" Minitusk.Config.pp config;
  
  (* Execute build *)
  match Minitusk.build ~root_dir ~config with
  | Ok () -> 
      Printf.printf "Build successful!\n%!";
      0
  | Error msg ->
      Printf.eprintf "Build failed: %s\n%!" msg;
      1

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s\n" usage_msg;
    Printf.eprintf "Commands:\n";
    Printf.eprintf "  build    Build all packages\n";
    exit 1
  );
  
  let exit_code = match Sys.argv.(1) with
    | "build" -> build_command ()
    | cmd ->
        Printf.eprintf "Unknown command: %s\n" cmd;
        Printf.eprintf "Usage: %s\n" usage_msg;
        1
  in
  
  exit exit_code