(** OCamldep wrapper - handles dependency analysis *)

(** Get the ocamldep binary path *)
let get_ocamldep_path (toolchain : Toolchains.t) =
  Filename.concat toolchain.bin_dir "ocamldep"

(** Sort ML/MLI files in dependency order *)
let sort ~toolchain ~cwd ~files =
  if files = [] then []
  else
    let ocamldep = get_ocamldep_path toolchain in
    let files_str = String.concat " " files in
    let cmd = Printf.sprintf "cd %s && %s -sort %s 2>/dev/null" cwd ocamldep files_str in
    
    let ic = Unix.open_process_in cmd in
    let sorted_str = try input_line ic with End_of_file -> "" in
    ignore (Unix.close_process_in ic);
    
    if sorted_str = "" then 
      files  (* Return original list if ocamldep fails *)
    else
      let sorted_basenames = String.split_on_char ' ' sorted_str in
      (* ocamldep -sort may not return all files if they have no dependencies *)
      (* So we need to add any missing files at the end *)
      let sorted_files = List.filter (fun f -> List.mem f sorted_basenames) files in
      let missing_files = List.filter (fun f -> not (List.mem f sorted_files)) files in
      sorted_files @ missing_files

(** Get dependencies for a single file *)
let deps ~toolchain ~cwd ~file =
  let ocamldep = get_ocamldep_path toolchain in
  let cmd = Printf.sprintf "cd %s && %s -modules %s 2>/dev/null" cwd ocamldep file in
  
  let ic = Unix.open_process_in cmd in
  let deps_str = try input_line ic with End_of_file -> "" in
  ignore (Unix.close_process_in ic);
  
  if deps_str = "" then []
  else
    (* Output format: "file.ml: Module1 Module2 Module3" *)
    match String.split_on_char ':' deps_str with
    | [_; deps_part] ->
        let deps = String.trim deps_part in
        if deps = "" then []
        else String.split_on_char ' ' deps |> List.map String.trim
    | _ -> []

(** Get all module dependencies (for building .merlin files) *)
let all_deps ~toolchain ~cwd ~files =
  List.map (fun file ->
    let deps = deps ~toolchain ~cwd ~file in
    (file, deps)
  ) files