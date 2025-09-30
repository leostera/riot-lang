open Std

open Model
(** OCamldep wrapper - handles dependency analysis *)

(** Sort ML/MLI files in dependency order *)
let sort ~toolchain ~cwd ~files =
  if files = [] then []
  else
    let ocamldep = Path.to_string (Toolchains.ocamldep_path toolchain) in
    let files_str = String.concat " " files in
    let cmd =
      Printf.sprintf "cd %s && %s -sort %s 2>/dev/null" cwd ocamldep files_str
    in

    Format.eprintf "[DEBUG Ocamldep] Running: %s@." cmd;

    let ic = Command.open_process_in cmd in
    let sorted_str = try input_line ic with End_of_file -> "" in
    ignore (Command.close_process_in ic);

    Format.eprintf "[DEBUG Ocamldep] Result: %s@." sorted_str;

    if sorted_str = "" then files (* Return original list if ocamldep fails *)
    else
      let sorted_basenames = String.split_on_char ' ' sorted_str in
      (* Filter out empty strings and return files in dependency order *)
      List.filter (fun s -> s <> "") sorted_basenames

(** Get dependencies for a single file - returns Module_name.t list *)
let deps ~toolchain ~cwd ~file ~package_namespace =
  let ocamldep = Path.to_string (Toolchains.ocamldep_path toolchain) in
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
        else
          String.split_on_char ' ' deps
          |> List.map String.trim
          |> List.map (fun modname ->
              (* Convert string module name to Module_name.t with proper namespace *)
              Model.Module_name.of_string ~namespace:package_namespace modname)
    | _ -> []

(** Get dependencies for a single file with optional flags - returns
    Module_name.t list *)
let deps_with_flags ~toolchain ~cwd ~file ~flags ~package_namespace =
  let ocamldep = Path.to_string (Toolchains.ocamldep_path toolchain) in
  let flags_str = Ocamlc.flags_to_string flags |> String.concat " " in
  (* Always include current directory so ocamldep can find .cmi files *)
  let cmd =
    Printf.sprintf "cd %s && %s -I . %s -modules %s 2>/dev/null" cwd ocamldep
      flags_str file
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
        else
          String.split_on_char ' ' deps
          |> List.map String.trim
          |> List.map (fun modname ->
              (* Convert string module name to Module_name.t with proper namespace *)
              Model.Module_name.of_string ~namespace:package_namespace modname)
    | _ -> []

(** Get all module dependencies (for building .merlin files) *)
let all_deps ~toolchain ~cwd ~files ~package_namespace =
  List.map
    (fun file ->
      let deps = deps ~toolchain ~cwd ~file ~package_namespace in
      (file, deps))
    files
