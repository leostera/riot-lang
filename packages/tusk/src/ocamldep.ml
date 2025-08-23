(** OCamldep wrapper - handles dependency analysis *)

(** Sort ML/MLI files in dependency order *)
let sort ~toolchain ~cwd ~files =
  if files = [] then []
  else
    let ocamldep = Toolchains.ocamldep_path toolchain in
    let files_str = String.concat " " files in
    let cmd =
      Printf.sprintf "cd %s && %s -sort %s 2>/dev/null" cwd ocamldep files_str
    in

    let ic = Command.open_process_in cmd in
    let sorted_str = try input_line ic with End_of_file -> "" in
    ignore (Command.close_process_in ic);

    if sorted_str = "" then files (* Return original list if ocamldep fails *)
    else
      let sorted_basenames = String.split_on_char ' ' sorted_str in
      (* ocamldep returns files in order, just return them directly *)
      sorted_basenames

(** Get dependencies for a single file *)
let deps ~toolchain ~cwd ~file =
  let ocamldep = Toolchains.ocamldep_path toolchain in
  let cmd =
    Printf.sprintf "cd %s && %s -modules %s 2>/dev/null" cwd ocamldep file
  in

  let ic = Command.open_process_in cmd in
  let deps_str = try input_line ic with End_of_file -> "" in
  ignore (Command.close_process_in ic);

  if deps_str = "" then []
  else
    (* Output format: "file.ml: Module1 Module2 Module3" *)
    match String.split_on_char ':' deps_str with
    | [ _; deps_part ] ->
        let deps = String.trim deps_part in
        if deps = "" then []
        else String.split_on_char ' ' deps |> List.map String.trim
    | _ -> []

(** Get all module dependencies (for building .merlin files) *)
let all_deps ~toolchain ~cwd ~files =
  List.map
    (fun file ->
      let deps = deps ~toolchain ~cwd ~file in
      (file, deps))
    files
