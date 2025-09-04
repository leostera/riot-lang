(** Minitusk - Minimal build system for tusk *)

(* Simple TOML parser for our needs *)
module Toml = struct
  let parse_file filename =
    let ic = open_in filename in
    let rec parse_lines acc =
      try
        let line = input_line ic in
        let trimmed = String.trim line in
        if String.length trimmed > 0 && trimmed.[0] <> '#' then
          parse_lines (trimmed :: acc)
        else parse_lines acc
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    parse_lines []

  let get_string_value lines key =
    List.find_map
      (fun line ->
        if String.starts_with ~prefix:(key ^ " = ") line then
          let value =
            String.sub line
              (String.length key + 3)
              (String.length line - String.length key - 3)
          in
          let value = String.trim value in
          if
            String.length value >= 2
            && value.[0] = '"'
            && value.[String.length value - 1] = '"'
          then Some (String.sub value 1 (String.length value - 2))
          else Some value
        else None)
      lines

  let get_array_value lines key =
    let rec find_array = function
      | [] -> []
      | line :: rest ->
          if String.starts_with ~prefix:(key ^ " = [") line then
            (* Handle inline array like: members = ["a", "b"] *)
            if String.contains line ']' then
              let start = String.index line '[' + 1 in
              let end_ = String.index line ']' in
              let content = String.sub line start (end_ - start) in
              parse_inline_array content
            else collect_array rest []
          else find_array rest
    and collect_array lines acc =
      match lines with
      | [] -> List.rev acc
      | line :: rest ->
          let trimmed = String.trim line in
          if trimmed = "]" then List.rev acc
          else if String.ends_with ~suffix:"]" trimmed then
            let item = String.sub trimmed 0 (String.length trimmed - 1) in
            List.rev (parse_array_item item :: acc)
          else collect_array rest (parse_array_item trimmed :: acc)
    and parse_array_item item =
      let item = String.trim item in
      let item =
        if String.ends_with ~suffix:"," item then
          String.sub item 0 (String.length item - 1)
        else item
      in
      let item = String.trim item in
      if
        String.length item >= 2
        && item.[0] = '"'
        && item.[String.length item - 1] = '"'
      then String.sub item 1 (String.length item - 2)
      else item
    and parse_inline_array content =
      let items = String.split_on_char ',' content in
      List.map parse_array_item items
    in
    find_array lines
end

(* Types *)
type package = { name : string; path : string; dependencies : string list }
type workspace = { packages : string list }

(* Read workspace tusk.toml *)
let read_workspace_config () =
  let lines = Toml.parse_file "tusk.toml" in
  (* Find lines after [workspace] section *)
  let rec find_workspace_section = function
    | [] -> []
    | line :: rest ->
        if line = "[workspace]" then rest (* Return lines after [workspace] *)
        else find_workspace_section rest
  in
  let workspace_lines = find_workspace_section lines in
  let members = Toml.get_array_value workspace_lines "members" in
  { packages = members }

(* Read package tusk.toml *)
let read_package_config path =
  let toml_path = Filename.concat path "tusk.toml" in
  if Sys.file_exists toml_path then
    let lines = Toml.parse_file toml_path in
    (* Find lines after [package] section *)
    let rec find_package_section = function
      | [] -> []
      | line :: rest ->
          if line = "[package]" then rest else find_package_section rest
    in
    let package_lines = find_package_section lines in
    let name =
      match Toml.get_string_value package_lines "name" with
      | Some n -> n
      | None -> Filename.basename path
    in
    (* Parse dependencies - find [dependencies] section *)
    let rec find_dependencies_section = function
      | [] -> []
      | line :: rest ->
          if line = "[dependencies]" then
            (* Collect until next section *)
            let rec collect_deps acc = function
              | [] -> List.rev acc
              | line :: rest ->
                  if String.length line > 0 && line.[0] = '[' then
                    List.rev acc (* Hit next section *)
                  else collect_deps (line :: acc) rest
            in
            collect_deps [] rest
          else find_dependencies_section rest
    in
    let dep_lines = find_dependencies_section lines in
    let dependencies =
      List.filter_map
        (fun line ->
          (* Look for lines like: miniriot = { path = "../miniriot" } *)
          if String.contains line '=' then
            let dep_name =
              String.trim (String.sub line 0 (String.index line '='))
            in
            if dep_name <> "" then Some dep_name else None
          else None)
        dep_lines
    in
    Some { name; path; dependencies }
  else None

(* Build dependency graph *)
let build_dependency_graph workspace =
  let real_packages =
    List.filter_map
      (fun pkg_path -> read_package_config pkg_path)
      workspace.packages
  in

  (* Inject fake "unix" package so dependency resolution works *)
  let unix_package = { name = "unix"; path = ""; dependencies = [] } in
  unix_package :: real_packages

(* Print dependency graph *)
let print_dependency_graph packages =
  Printf.printf "=== Workspace Dependency Graph ===\n\n%!";
  List.iter
    (fun pkg ->
      Printf.printf "Package: %s (at %s)\n%!" pkg.name pkg.path;
      if List.length pkg.dependencies > 0 then (
        Printf.printf "  Dependencies:\n%!";
        List.iter (fun dep -> Printf.printf "    - %s\n%!" dep) pkg.dependencies)
      else Printf.printf "  No dependencies\n%!";
      Printf.printf "\n%!")
    packages

(* Topological sort *)
let topological_sort packages =
  (* Build adjacency list and in-degree count *)
  let pkg_map =
    List.fold_left
      (fun acc pkg ->
        List.assoc_opt pkg.name acc |> function
        | None -> (pkg.name, pkg) :: acc
        | Some _ -> acc)
      [] packages
  in

  let in_degree = Hashtbl.create 16 in
  let adj_list = Hashtbl.create 16 in

  (* Initialize *)
  List.iter
    (fun pkg ->
      Hashtbl.replace in_degree pkg.name 0;
      Hashtbl.replace adj_list pkg.name [])
    packages;

  (* Build graph *)
  List.iter
    (fun pkg ->
      List.iter
        (fun dep ->
          (* Add edge from dep to pkg *)
          let deps = try Hashtbl.find adj_list dep with Not_found -> [] in
          Hashtbl.replace adj_list dep (pkg.name :: deps);
          (* Increment in-degree *)
          let deg = try Hashtbl.find in_degree pkg.name with Not_found -> 0 in
          Hashtbl.replace in_degree pkg.name (deg + 1))
        pkg.dependencies)
    packages;

  (* Kahn's algorithm *)
  let queue = Queue.create () in
  Hashtbl.iter (fun name deg -> if deg = 0 then Queue.add name queue) in_degree;

  let sorted = ref [] in
  while not (Queue.is_empty queue) do
    let name = Queue.take queue in
    sorted := name :: !sorted;

    let neighbors = try Hashtbl.find adj_list name with Not_found -> [] in
    List.iter
      (fun neighbor ->
        let deg = Hashtbl.find in_degree neighbor in
        Hashtbl.replace in_degree neighbor (deg - 1);
        if deg - 1 = 0 then Queue.add neighbor queue)
      neighbors
  done;

  (* Return packages in build order *)
  List.rev !sorted |> List.filter_map (fun name -> List.assoc_opt name pkg_map)

(* Generate namespaced module name *)
let get_namespaced_name ~package_name ~file_path =
  let basename = Filename.basename file_path in
  let name_without_ext =
    if Filename.check_suffix basename ".ml" then
      Filename.chop_suffix basename ".ml"
    else if Filename.check_suffix basename ".mli" then
      Filename.chop_suffix basename ".mli"
    else basename
  in
  (* Replace hyphens with underscores in package name *)
  let safe_package_name =
    String.map (fun c -> if c = '-' then '_' else c) package_name
  in

  (* Get folder structure relative to src/ *)
  let rec get_folder_parts path acc =
    let dir = Filename.dirname path in
    let base = Filename.basename dir in
    if base = "src" || dir = "." || dir = "/" then acc
    else get_folder_parts dir (base :: acc)
  in
  let folder_parts = get_folder_parts file_path [] in

  (* Check if this is a folder interface (e.g., cli/cli.ml) *)
  let is_folder_interface =
    match List.rev folder_parts with
    | folder :: _ when folder = name_without_ext -> true
    | _ -> false
  in

  (* Build the full namespaced name *)
  if name_without_ext = safe_package_name && folder_parts = [] then
    (* Main package module *)
    String.capitalize_ascii name_without_ext
  else if folder_parts = [] then
    (* Top-level module *)
    String.capitalize_ascii safe_package_name
    ^ "__"
    ^ String.capitalize_ascii name_without_ext
  else if is_folder_interface then
    (* Folder interface module (e.g., cli/cli.ml -> Tusk__Cli) *)
    String.capitalize_ascii safe_package_name
    ^ "__"
    ^ String.concat "__" (List.map String.capitalize_ascii folder_parts)
  else
    (* Module in a folder (e.g., cli/build.ml -> Tusk__Cli__Build) *)
    String.capitalize_ascii safe_package_name
    ^ "__"
    ^ String.concat "__" (List.map String.capitalize_ascii folder_parts)
    ^ "__"
    ^ String.capitalize_ascii name_without_ext

(* Collect source files *)
let collect_sources pkg_path =
  let src_dir = Filename.concat pkg_path "src" in

  let rec collect_files dir acc =
    if Sys.file_exists dir && Sys.is_directory dir then
      let files = Sys.readdir dir in
      Array.fold_left
        (fun acc file ->
          let path = Filename.concat dir file in
          if Sys.is_directory path then collect_files path acc else path :: acc)
        acc files
    else acc
  in

  let all_files = collect_files src_dir [] in

  (* Separate ML and C files *)
  let is_ml_file f =
    Filename.check_suffix f ".ml" || Filename.check_suffix f ".mli"
  in
  let is_c_file f = Filename.check_suffix f ".c" in

  let ml_sources =
    List.filter (fun f -> is_ml_file (Filename.basename f)) all_files
  in
  let c_sources =
    List.filter (fun f -> is_c_file (Filename.basename f)) all_files
  in

  (ml_sources, c_sources)

(* Convert snake_case to CamelCase *)
let snake_to_camel s =
  s |> String.split_on_char '_'
  |> List.map String.capitalize_ascii
  |> String.concat ""

(* Determine expected outputs *)
let expected_outputs pkg sources c_sources =
  (* All packages produce .cma and .cmi files *)
  let outputs = ref [] in

  outputs := (pkg.name ^ ".cma") :: !outputs;
  outputs := (pkg.name ^ ".cmi") :: !outputs;

  !outputs

(* Compute SHA512 *)
let sha512_file path =
  let ic = open_in_bin path in
  let digest = Digest.file path in
  close_in ic;
  Digest.to_hex digest

let sha512_string str = Digest.to_hex (Digest.string str)

(* Build a package *)
let build_package packages pkg sources c_sources outputs transitive_deps =
  (* Compute build hash *)
  let source_hashes = List.map sha512_file (sources @ c_sources) in
  let all_hashes = source_hashes @ transitive_deps in
  let commands_hash = sha512_string "ocamlc" in
  (* Simplified for now *)
  let build_hash =
    sha512_string (String.concat "" (all_hashes @ [ commands_hash ]))
  in

  let sandbox_dir = Printf.sprintf "./target/bootstrap/sandbox/%s" build_hash in
  let out_dir = "./target/bootstrap/out" in

  (* Create directories *)
  let rec mkdir_p dir =
    if not (Sys.file_exists dir) then (
      mkdir_p (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  mkdir_p sandbox_dir;
  mkdir_p out_dir;

  Printf.printf "\nBuilding %s in %s\n%!" pkg.name sandbox_dir;
  Printf.printf "  Sources: %d ML, %d C\n%!" (List.length sources)
    (List.length c_sources);
  Printf.printf "  Expected outputs: %s\n%!" (String.concat ", " outputs);

  (* Copy sources to sandbox, renaming lib.ml/lib.mli to package name *)
  (* For packages with single module, also create package interface *)
  let has_lib_ml =
    List.exists (fun src -> Filename.basename src = "lib.ml") sources
  in

  List.iter
    (fun src ->
      let basename = Filename.basename src in
      (* Get relative path from src/ directory *)
      let src_dir = Filename.concat pkg.path "src" in
      let relative_path =
        (* Remove the package src directory prefix to get relative path *)
        if String.starts_with ~prefix:(src_dir ^ "/") src then
          String.sub src
            (String.length src_dir + 1)
            (String.length src - String.length src_dir - 1)
        else basename
      in

      let dest_path =
        if basename = "lib.ml" then
          Filename.concat sandbox_dir (pkg.name ^ ".ml")
        else if basename = "lib.mli" then
          Filename.concat sandbox_dir (pkg.name ^ ".mli")
        else Filename.concat sandbox_dir relative_path
      in

      (* Create parent directory if needed *)
      let dest_dir = Filename.dirname dest_path in
      (if dest_dir <> sandbox_dir && dest_dir <> "." then
         let cmd = Printf.sprintf "mkdir -p %s" dest_dir in
         ignore (Unix.system cmd));

      let cmd = Printf.sprintf "cp %s %s" src dest_path in
      ignore (Unix.system cmd))
    (sources @ c_sources);

  (* Also copy header files from the source directory *)
  let src_dir = Filename.concat pkg.path "src" in
  let header_files =
    try
      Array.to_list (Sys.readdir src_dir)
      |> List.filter (fun f -> Filename.check_suffix f ".h")
      |> List.map (fun f -> Filename.concat src_dir f)
    with _ -> []
  in
  List.iter
    (fun header ->
      let basename = Filename.basename header in
      let dest = Filename.concat sandbox_dir basename in
      let cmd = Printf.sprintf "cp %s %s" header dest in
      ignore (Unix.system cmd))
    header_files;

  (* For single-module packages, create package interface *)
  (if (not has_lib_ml) && List.length sources = 1 then (
     let src = List.hd sources in
     let basename = Filename.basename src in
     if Filename.check_suffix basename ".ml" then (
       let cmd = Printf.sprintf "cp %s %s/%s.ml" src sandbox_dir pkg.name in
       ignore (Unix.system cmd);
       (* Also copy .mli if it exists *)
       let mli_src = Filename.chop_suffix src ".ml" ^ ".mli" in
       if List.exists (fun s -> s = mli_src) sources then
         let cmd =
           Printf.sprintf "cp %s %s/%s.mli" mli_src sandbox_dir pkg.name
         in
         ignore (Unix.system cmd)))
   else if (not has_lib_ml) && List.length sources = 2 then
     (* Handle case with .ml and .mli *)
     let ml_file =
       List.find_opt (fun s -> Filename.check_suffix s ".ml") sources
     in
     let mli_file =
       List.find_opt (fun s -> Filename.check_suffix s ".mli") sources
     in
     match (ml_file, mli_file) with
     | Some ml, Some mli ->
         let cmd = Printf.sprintf "cp %s %s/%s.ml" ml sandbox_dir pkg.name in
         ignore (Unix.system cmd);
         let cmd = Printf.sprintf "cp %s %s/%s.mli" mli sandbox_dir pkg.name in
         ignore (Unix.system cmd)
     | _ -> ());

  (* Copy all transitive dependencies' outputs to sandbox *)
  let rec get_all_deps dep_name visited =
    if List.mem dep_name visited then visited
    else
      match List.find_opt (fun p -> p.name = dep_name) packages with
      | None -> dep_name :: visited
      | Some dep_pkg ->
          let visited = dep_name :: visited in
          List.fold_left
            (fun acc d -> get_all_deps d acc)
            visited dep_pkg.dependencies
  in
  let all_dep_names =
    List.fold_left (fun acc dep -> get_all_deps dep acc) [] pkg.dependencies
    |> List.rev
  in

  List.iter
    (fun dep_name ->
      let dep_out = Filename.concat out_dir dep_name in
      if Sys.file_exists dep_out && Sys.is_directory dep_out then (
        (* Copy all .cmi and .cma files, plus .cmo for non-library modules *)
        let cmd =
          Printf.sprintf "cp %s/*.cmi %s/ 2>/dev/null || true" dep_out
            sandbox_dir
        in
        ignore (Unix.system cmd);
        let cmd =
          Printf.sprintf "cp %s/*.cma %s/ 2>/dev/null || true" dep_out
            sandbox_dir
        in
        ignore (Unix.system cmd);
        (* For standalone modules without lib.ml, also copy .cmo files *)
        let cmd =
          Printf.sprintf "cp %s/*.cmo %s/ 2>/dev/null || true" dep_out
            sandbox_dir
        in
        ignore (Unix.system cmd);
        (* Also copy C object files for packages with C stubs *)
        let cmd =
          Printf.sprintf "cp %s/*.o %s/ 2>/dev/null || true" dep_out sandbox_dir
        in
        ignore (Unix.system cmd)))
    all_dep_names;

  (* Build include paths for dependencies - just need -I . once since all are in same sandbox *)
  let dep_includes = "" in

  (* Get OCaml toolchain *)
  let home = Sys.getenv "HOME" in
  let ocamlc = Printf.sprintf "%s/.tusk/toolchains/5.3.0/bin/ocamlc" home in

  (* Compile C files *)
  List.iter
    (fun c_src ->
      let c_file = Filename.basename c_src in
      let cmd =
        Printf.sprintf "cd %s && %s -I +unix -c %s" sandbox_dir ocamlc c_file
      in
      Printf.printf "  $ %s\n%!" cmd;
      let ret = Unix.system cmd in
      if ret <> Unix.WEXITED 0 then (
        Printf.printf "Error: Command failed with %s\n%!"
          (match ret with
          | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
          | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
          | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
        exit 1))
    c_sources;

  (* Use ocamldep to sort ML and MLI files together *)
  let ml_files = List.filter (fun f -> Filename.check_suffix f ".ml") sources in
  let mli_files =
    List.filter (fun f -> Filename.check_suffix f ".mli") sources
  in
  let all_ml_files = mli_files @ ml_files in
  (* Process .mli files before .ml files *)

  let sorted_all_files =
    if all_ml_files = [] then []
    else
      (* Run ocamldep to get dependencies on all files *)
      (* Map source files to their sandbox names *)
      let source_to_sandbox_name src =
        let basename = Filename.basename src in
        let src_dir = Filename.concat pkg.path "src" in

        if basename = "lib.ml" then pkg.name ^ ".ml"
        else if basename = "lib.mli" then pkg.name ^ ".mli"
        else if String.starts_with ~prefix:(src_dir ^ "/") src then
          (* Get relative path from src/ *)
          String.sub src
            (String.length src_dir + 1)
            (String.length src - String.length src_dir - 1)
        else basename
      in
      let ocamldep =
        Printf.sprintf "%s/.tusk/toolchains/5.3.0/bin/ocamldep" home
      in
      let cmd =
        Printf.sprintf
          "cd %s && find . -name '*.ml' -o -name '*.mli' | xargs %s -I . -sort \
           2>/dev/null"
          sandbox_dir ocamldep
      in
      let ic = Unix.open_process_in cmd in
      let sorted_str = try input_line ic with End_of_file -> "" in
      ignore (Unix.close_process_in ic);

      Printf.printf "  OCamldep returned: %s\n%!" sorted_str;

      (* Parse the sorted output and map back to original filenames *)
      if sorted_str = "" then all_ml_files
      else
        let sorted_basenames = String.split_on_char ' ' sorted_str in
        (* ocamldep returns paths relative to sandbox dir, map back to source files *)
        let sorted_files =
          List.filter_map
            (fun sandbox_name ->
              (* Remove ./ prefix if present *)
              let clean_name =
                if String.starts_with ~prefix:"./" sandbox_name then
                  String.sub sandbox_name 2 (String.length sandbox_name - 2)
                else sandbox_name
              in
              (* Find the original source file that maps to this sandbox file *)
              List.find_opt
                (fun src -> source_to_sandbox_name src = clean_name)
                all_ml_files)
            sorted_basenames
        in
        (* Add any files that weren't in the sorted output at the end *)
        let missing_files =
          List.filter (fun f -> not (List.mem f sorted_files)) all_ml_files
        in
        sorted_files @ missing_files
  in

  (* Split back into mli and ml files in sorted order *)
  let sorted_mli_files =
    List.filter (fun f -> Filename.check_suffix f ".mli") sorted_all_files
  in
  let sorted_ml_files =
    List.filter (fun f -> Filename.check_suffix f ".ml") sorted_all_files
  in

  (* Debug: show sorted order *)
  if sorted_all_files <> [] then
    Printf.printf "  Sorted build order: %s\n%!"
      (String.concat ", " (List.map Filename.basename sorted_all_files));

  (* Generate alias module if we have namespaced modules *)
  let alias_module_name_opt =
    (* Collect all modules that need aliases *)
    let module_aliases =
      sorted_ml_files @ sorted_mli_files
      |> List.filter_map (fun file ->
          let basename = Filename.basename file in
          let name_without_ext =
            if Filename.check_suffix basename ".ml" then
              Filename.chop_suffix basename ".ml"
            else if Filename.check_suffix basename ".mli" then
              Filename.chop_suffix basename ".mli"
            else basename
          in

          (* Get folder structure for simple name *)
          let rec get_folder_parts path acc =
            let dir = Filename.dirname path in
            let base = Filename.basename dir in
            if base = "src" || dir = "." || dir = "/" then acc
            else get_folder_parts dir (base :: acc)
          in
          let folder_parts = get_folder_parts file [] in

          (* Build simple name with folder structure *)
          let simple_name =
            if folder_parts = [] then String.capitalize_ascii name_without_ext
            else
              (* Check if it's a folder interface *)
              let is_folder_interface =
                match List.rev folder_parts with
                | folder :: _ when folder = name_without_ext -> true
                | _ -> false
              in
              if is_folder_interface then
                String.concat "."
                  (List.map String.capitalize_ascii folder_parts)
              else
                String.concat "."
                  (List.map String.capitalize_ascii folder_parts
                  @ [ String.capitalize_ascii name_without_ext ])
          in

          let namespaced =
            get_namespaced_name ~package_name:pkg.name ~file_path:file
          in

          (* Folder interface modules (like cli/cli.ml) should NOT point to __aliases *)
          (* They ARE the folder module themselves *)
          let target_module = namespaced in

          (* Skip if it's the main package module or if names are the same *)
          if simple_name = namespaced then None
          else Some (simple_name, target_module))
      |> List.sort_uniq compare (* Remove duplicates *)
    in

    if module_aliases <> [] then (
      (* Generate alias module content *)
      (* Convert dot notation to simple names for OCaml syntax *)
      let alias_content =
        "(* Auto-generated module aliases for package " ^ pkg.name ^ " *)\n"
        ^ (List.map
             (fun (simple, namespaced) ->
               (* If simple name contains a dot, we need to convert it to a valid OCaml module name *)
               if String.contains simple '.' then
                 (* For Cli.Build, generate: module Build = Tusk__Cli__Build *)
                 let last_dot = String.rindex simple '.' in
                 let module_name =
                   String.sub simple (last_dot + 1)
                     (String.length simple - last_dot - 1)
                 in
                 Printf.sprintf "module %s = %s" module_name namespaced
               else Printf.sprintf "module %s = %s" simple namespaced)
             module_aliases
          |> String.concat "\n")
      in

      (* Create a unique alias module name *)
      let safe_name =
        String.map (fun c -> if c = '-' then '_' else c) pkg.name
      in
      let alias_module_name = String.capitalize_ascii safe_name ^ "__aliases" in
      let alias_ml = alias_module_name ^ ".ml" in

      (* Write alias module file *)
      let alias_file_path = Filename.concat sandbox_dir alias_ml in
      let oc = open_out alias_file_path in
      output_string oc alias_content;
      close_out oc;

      (* Compile the alias module with -no-alias-deps and suppress warning 49 *)
      let alias_cmo = alias_module_name ^ ".cmo" in
      let cmd =
        Printf.sprintf
          "cd %s && %s -I +unix -I . %s -no-alias-deps -w -49 -c -o %s %s"
          sandbox_dir ocamlc dep_includes alias_cmo alias_ml
      in
      Printf.printf "  $ %s\n%!" cmd;
      let ret = Unix.system cmd in
      if ret <> Unix.WEXITED 0 then (
        Printf.printf "Error: Command failed with %s\n%!"
          (match ret with
          | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
          | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
          | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
        exit 1);

      Some alias_module_name)
    else None
  in

  (* Build -open flag if we have an alias module *)
  let open_flag =
    match alias_module_name_opt with
    | Some name -> " -open " ^ name
    | None -> ""
  in

  (* Compile ML interfaces in sorted order with namespacing *)
  List.iter
    (fun mli_src ->
      let basename = Filename.basename mli_src in
      let src_dir = Filename.concat pkg.path "src" in
      (* Get the file path in sandbox *)
      let mli_file =
        if basename = "lib.mli" then pkg.name ^ ".mli"
        else if String.starts_with ~prefix:(src_dir ^ "/") mli_src then
          String.sub mli_src
            (String.length src_dir + 1)
            (String.length mli_src - String.length src_dir - 1)
        else basename
      in
      (* Generate namespaced output name *)
      let namespaced =
        get_namespaced_name ~package_name:pkg.name ~file_path:mli_src
      in
      let cmi_output = namespaced ^ ".cmi" in
      (* Compile all .mli files with -open flag if we have aliases *)
      let cmd =
        Printf.sprintf "cd %s && %s -I +unix -I . %s%s -c -o %s %s" sandbox_dir
          ocamlc dep_includes open_flag cmi_output mli_file
      in
      Printf.printf "  $ %s\n%!" cmd;
      let ret = Unix.system cmd in
      if ret <> Unix.WEXITED 0 then (
        Printf.printf "Error: Command failed with %s\n%!"
          (match ret with
          | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
          | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
          | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
        exit 1))
    sorted_mli_files;

  (* Compile ML implementations in sorted order with namespacing *)
  List.iter
    (fun ml_src ->
      let basename = Filename.basename ml_src in
      let src_dir = Filename.concat pkg.path "src" in
      (* Get the file path in sandbox *)
      let ml_file =
        if basename = "lib.ml" then pkg.name ^ ".ml"
        else if String.starts_with ~prefix:(src_dir ^ "/") ml_src then
          String.sub ml_src
            (String.length src_dir + 1)
            (String.length ml_src - String.length src_dir - 1)
        else basename
      in
      (* Generate namespaced output name *)
      let namespaced =
        get_namespaced_name ~package_name:pkg.name ~file_path:ml_src
      in
      let cmo_output = namespaced ^ ".cmo" in
      (* Include all C objects when compiling ML files with external declarations *)
      let c_objects =
        if c_sources <> [] && basename = pkg.name ^ ".ml" then
          String.concat " "
            (List.map
               (fun c_src ->
                 let base =
                   Filename.chop_suffix (Filename.basename c_src) ".c"
                 in
                 base ^ ".o")
               c_sources)
        else ""
      in
      let cmd =
        Printf.sprintf "cd %s && %s -I +unix -I . %s%s -c -o %s %s %s"
          sandbox_dir ocamlc dep_includes open_flag cmo_output ml_file c_objects
      in
      Printf.printf "  $ %s\n%!" cmd;
      let ret = Unix.system cmd in
      if ret <> Unix.WEXITED 0 then (
        Printf.printf "Error: Command failed with %s\n%!"
          (match ret with
          | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
          | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
          | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
        exit 1))
    sorted_ml_files;

  (* Create library .cma file for all packages *)
  (* For linking, we need modules in dependency order from sorted_ml_files *)
  (* ocamldep -sort gives us compilation order, which is also the correct linking order for .ml files *)
  let cmo_files =
    (* Add alias module first if it exists *)
    let alias_cmo_list =
      match alias_module_name_opt with
      | Some name -> [ name ^ ".cmo" ]
      | None -> []
    in
    let ml_cmos =
      List.map
        (fun f ->
          let namespaced =
            get_namespaced_name ~package_name:pkg.name ~file_path:f
          in
          namespaced ^ ".cmo")
        sorted_ml_files
    in
    (* Deduplicate the list while preserving order *)
    let deduplicate lst =
      let seen = Hashtbl.create 10 in
      List.filter
        (fun x ->
          if Hashtbl.mem seen x then false
          else (
            Hashtbl.add seen x true;
            true))
        lst
    in
    deduplicate (alias_cmo_list @ ml_cmos)
  in

  (* If there's a module with the same name as the package, move it to the end *)
  (* This handles the case where a module re-exports others *)
  let safe_pkg_name =
    String.map (fun c -> if c = '-' then '_' else c) pkg.name
  in
  let main_module_cmo = String.capitalize_ascii safe_pkg_name ^ ".cmo" in
  let cmo_files =
    if List.mem main_module_cmo cmo_files then
      let others = List.filter (fun f -> f <> main_module_cmo) cmo_files in
      others @ [ main_module_cmo ]
    else cmo_files
  in

  let cmo_list = String.concat " " cmo_files in
  Printf.printf "  Linking order: %s\n%!" cmo_list;
  (* Include C object files in the archive *)
  let c_obj_files =
    List.map
      (fun c_src ->
        let base = Filename.chop_suffix (Filename.basename c_src) ".c" in
        base ^ ".o")
      c_sources
  in
  let c_obj_list = String.concat " " c_obj_files in
  let all_objects =
    if c_obj_list = "" then cmo_list else cmo_list ^ " " ^ c_obj_list
  in
  let cmd =
    Printf.sprintf "cd %s && %s -a -o %s.cma %s" sandbox_dir ocamlc pkg.name
      all_objects
  in
  Printf.printf "  $ %s\n%!" cmd;
  let ret = Unix.system cmd in
  if ret <> Unix.WEXITED 0 then (
    Printf.printf "Error: Command failed with %s\n%!"
      (match ret with
      | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
      | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
      | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
    exit 1);

  (* C objects are now embedded in .cmo files, no separate static library needed *)

  (* Copy outputs to out_dir *)
  let pkg_out_dir = Filename.concat out_dir pkg.name in
  mkdir_p pkg_out_dir;

  (* Copy all expected outputs *)
  List.iter
    (fun out ->
      let src_file = Filename.concat sandbox_dir out in
      if Sys.file_exists src_file then (
        let cmd = Printf.sprintf "cp %s %s/" src_file pkg_out_dir in
        Printf.printf "  $ %s\n%!" cmd;
        let ret = Unix.system cmd in
        if ret <> Unix.WEXITED 0 then (
          Printf.printf "Error: Command failed with %s\n%!"
            (match ret with
            | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
            | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
            | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
          exit 1)))
    outputs;

  (* Also copy all .cmi files for multi-module packages (needed for module access) *)
  let cmd =
    Printf.sprintf "cp %s/*.cmi %s/ 2>/dev/null || true" sandbox_dir pkg_out_dir
  in
  Printf.printf "  $ %s\n%!" cmd;
  ignore (Unix.system cmd);

  (* Also copy C object files for packages with C stubs *)
  if c_sources <> [] then
    List.iter
      (fun c_src ->
        let obj_name =
          Filename.chop_suffix (Filename.basename c_src) ".c" ^ ".o"
        in
        let src_file = Filename.concat sandbox_dir obj_name in
        if Sys.file_exists src_file then (
          let cmd = Printf.sprintf "cp %s %s/" src_file pkg_out_dir in
          Printf.printf "  $ %s\n%!" cmd;
          ignore (Unix.system cmd)))
      c_sources;

  (* Build executable if main.ml exists *)
  let has_main_ml =
    List.exists (fun src -> Filename.basename src = "main.ml") sources
  in
  if has_main_ml then (
    (* Link all .cmo files and dependencies into executable *)
    let all_cmos =
      List.map
        (fun src ->
          let namespaced =
            get_namespaced_name ~package_name:pkg.name ~file_path:src
          in
          namespaced ^ ".cmo")
        sorted_ml_files
    in

    (* Get all transitive dependencies for linking *)
    let all_deps = all_dep_names in
    (* Use the same list we computed for copying *)

    (* Group packages by name - each package's .cma and .cmo together *)
    let dep_files = ref [] in

    (* Process dependencies in topological order (dependencies first) *)
    let sorted_deps =
      List.filter
        (fun pkg -> List.mem pkg.name all_deps)
        (topological_sort packages)
    in

    List.iter
      (fun pkg ->
        let dep_name = pkg.name in
        let cma_path = Printf.sprintf "%s.cma" dep_name in

        if Sys.file_exists (Filename.concat sandbox_dir cma_path) then
          (* All packages now produce .cma files *)
          dep_files := !dep_files @ [ cma_path ])
      sorted_deps;

    let link_deps = String.concat " " !dep_files in
    let link_objs = String.concat " " all_cmos in

    (* C objects are now embedded in .cma files, no separate static library linking needed *)

    (* Correct OCaml link order: dependencies grouped by package, then current modules *)
    (* Use -custom flag when linking with C stubs *)
    let has_c_stubs =
      c_sources <> []
      || List.exists
           (fun dep_pkg ->
             let _, dep_c_sources = collect_sources dep_pkg.path in
             dep_c_sources <> [])
           sorted_deps
    in
    let custom_flag = if has_c_stubs then "-custom" else "" in
    (* C objects are already embedded in .cma files, no need to link them separately *)
    let cmd =
      Printf.sprintf "cd %s && %s %s -I +unix -I . %s -o %s unix.cma %s %s"
        sandbox_dir ocamlc custom_flag dep_includes pkg.name link_deps link_objs
    in
    Printf.printf "  $ %s\n%!" cmd;
    let ret = Unix.system cmd in
    if ret <> Unix.WEXITED 0 then (
      Printf.printf "Error: Command failed with %s\n%!"
        (match ret with
        | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
        | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
        | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
      exit 1);

    (* Copy executable to package out dir *)
    let exe_path = Filename.concat sandbox_dir pkg.name in
    let cmd = Printf.sprintf "cp %s %s/" exe_path pkg_out_dir in
    Printf.printf "  $ %s\n%!" cmd;
    let ret = Unix.system cmd in
    if ret <> Unix.WEXITED 0 then (
      Printf.printf "Error: Command failed with %s\n%!"
        (match ret with
        | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
        | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
        | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
      exit 1);

    (* Also copy to ./target/bootstrap/<package-name> *)
    let target_exe = Printf.sprintf "./target/bootstrap/%s" pkg.name in
    let cmd = Printf.sprintf "cp %s %s" exe_path target_exe in
    Printf.printf "  $ %s\n%!" cmd;
    let ret = Unix.system cmd in
    if ret <> Unix.WEXITED 0 then (
      Printf.printf "Error: Command failed with %s\n%!"
        (match ret with
        | Unix.WEXITED n -> Printf.sprintf "exit code %d" n
        | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
        | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n);
      exit 1));

  (* Return output hashes for dependencies *)
  List.map (fun out -> sha512_string out) outputs

(* Main *)
let () =
  Printf.printf "=== Minitusk Build System ===\n\n%!";

  (* Step 1: Read top-level tusk.toml *)
  Printf.printf "Reading workspace configuration...\n%!";
  let workspace = read_workspace_config () in
  Printf.printf "Found %d packages\n\n%!" (List.length workspace.packages);

  (* Step 2: List all packages *)
  Printf.printf "Packages in workspace:\n%!";
  List.iter (fun pkg -> Printf.printf "  - %s\n%!" pkg) workspace.packages;
  Printf.printf "\n%!";

  (* Step 3: Read each package's tusk.toml *)
  Printf.printf "Reading package configurations...\n%!";
  let packages = build_dependency_graph workspace in

  (* Step 4 & 5: Build and print dependency graph *)
  print_dependency_graph packages;

  (* Step 6: Topological sort *)
  Printf.printf "=== Build Order ===\n\n%!";
  let build_order = topological_sort packages in
  List.iteri
    (fun i pkg -> Printf.printf "%d. %s\n%!" (i + 1) pkg.name)
    build_order;

  (* Step 7: Build packages *)
  Printf.printf "\n=== Building Packages ===\n%!";
  let built_outputs = Hashtbl.create 16 in

  List.iter
    (fun pkg ->
      (* Skip fake unix package *)
      if pkg.name = "unix" then
        Printf.printf "\nSkipping external package: %s\n%!" pkg.name
      else (
        Printf.printf "\n";
        (* Collect sources *)
        let sources, c_sources = collect_sources pkg.path in

        (* Determine outputs *)
        let outputs = expected_outputs pkg sources c_sources in

        (* Collect transitive dependencies *)
        let transitive_deps =
          let rec collect pkg_name acc =
            match List.find_opt (fun p -> p.name = pkg_name) packages with
            | None -> acc
            | Some p ->
                let dep_outputs =
                  try Hashtbl.find built_outputs p.name with Not_found -> []
                in
                List.fold_left
                  (fun acc dep -> collect dep (dep_outputs @ acc))
                  (dep_outputs @ acc) p.dependencies
          in
          List.fold_left (fun acc dep -> collect dep acc) [] pkg.dependencies
        in

        (* Build *)
        let output_hashes =
          build_package packages pkg sources c_sources outputs transitive_deps
        in
        Hashtbl.replace built_outputs pkg.name output_hashes))
    build_order
