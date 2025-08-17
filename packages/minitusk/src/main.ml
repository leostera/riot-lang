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
      let dest =
        if basename = "lib.ml" then
          Filename.concat sandbox_dir (pkg.name ^ ".ml")
        else if basename = "lib.mli" then
          Filename.concat sandbox_dir (pkg.name ^ ".mli")
        else Filename.concat sandbox_dir basename
      in
      let cmd = Printf.sprintf "cp %s %s" src dest in
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

  (* Build include paths for dependencies *)
  let dep_includes =
    String.concat " " (List.map (fun dep -> "-I .") all_dep_names)
  in

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
      let files_str =
        String.concat " "
          (List.map (fun f -> Filename.basename f) all_ml_files)
      in
      let ocamldep =
        Printf.sprintf "%s/.tusk/toolchains/5.3.0/bin/ocamldep" home
      in
      let cmd =
        Printf.sprintf "cd %s && %s -sort %s 2>/dev/null" sandbox_dir ocamldep
          files_str
      in
      let ic = Unix.open_process_in cmd in
      let sorted_str = try input_line ic with End_of_file -> "" in
      ignore (Unix.close_process_in ic);

      (* Parse the sorted output and map back to original filenames *)
      if sorted_str = "" then all_ml_files
      else
        let sorted_basenames = String.split_on_char ' ' sorted_str in
        (* ocamldep -sort might not include all files, so we need to handle that *)
        let sorted_files = List.filter_map
          (fun basename ->
            List.find_opt
              (fun f -> Filename.basename f = basename)
              all_ml_files)
          sorted_basenames
        in
        (* Add any files that weren't in the sorted output at the end *)
        let missing_files = List.filter
          (fun f -> not (List.mem f sorted_files))
          all_ml_files
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

  (* Compile ML interfaces in sorted order with namespacing *)
  List.iter
    (fun mli_src ->
      let basename = Filename.basename mli_src in
      let mli_file =
        if basename = "lib.mli" then pkg.name ^ ".mli" else basename
      in
      (* Compile all .mli files *)
      let cmd =
        Printf.sprintf "cd %s && %s -I +unix -I . %s -c %s" sandbox_dir ocamlc
          dep_includes mli_file
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

  (* Compile ML implementations in sorted order *)
  List.iter
    (fun ml_src ->
      let basename = Filename.basename ml_src in
      let ml_file =
        if basename = "lib.ml" then pkg.name ^ ".ml" else basename
      in
      (* Include all C objects when compiling ML files with external declarations *)
      let c_objects =
        if c_sources <> [] && basename = "lib.ml" then
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
        Printf.sprintf "cd %s && %s -I +unix -I . %s -c %s %s" sandbox_dir
          ocamlc dep_includes ml_file c_objects
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
  let cmo_files =
    List.map
      (fun src ->
        let basename = Filename.basename src in
        let base =
          if basename = "lib.ml" then pkg.name
          else Filename.chop_suffix basename ".ml"
        in
        base ^ ".cmo")
      sorted_ml_files
  in
  let cmo_list = String.concat " " cmo_files in
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
          let base = Filename.chop_suffix (Filename.basename src) ".ml" in
          base ^ ".cmo")
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
