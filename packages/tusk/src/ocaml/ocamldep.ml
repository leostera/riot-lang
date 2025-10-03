open Std

open Model
(** OCamldep wrapper - handles dependency analysis *)

(** Sort ML/MLI files in dependency order *)
let sort ~toolchain ~cwd ~files =
  if files = [] then []
  else
    let ocamldep = Path.to_string (Toolchains.ocamldep_path toolchain) in
    let files_str = String.concat " " (List.map Path.to_string files) in
    let cmd =
      format "cd %s && %s -sort %s 2>/dev/null" (Path.to_string cwd) ocamldep files_str
    in

    Log.debug "  $ %s" cmd;

    let sorted_str =
      let command = Command.make ~args:[ "-c"; cmd ] "sh" in
      match Command.output command with
      | Ok output -> (
          match String.split_on_char '\n' output.Command.stdout with
          | line :: _ -> String.trim line
          | [] -> "")
      | Error _ -> ""
    in

    Log.debug "  [ocamldep] Result: %s" sorted_str;

    if sorted_str = "" then files (* Return original list if ocamldep fails *)
    else
      let sorted_basenames = String.split_on_char ' ' sorted_str in
      (* Filter out empty strings and return files in dependency order *)
      List.filter_map (fun s -> if s = "" then None else Some (Path.v s)) sorted_basenames

(** Get dependencies for a single file - returns Module_name.t list *)
let deps ~toolchain ~cwd ~file ~package_namespace =
  let ocamldep = Path.to_string (Toolchains.ocamldep_path toolchain) in
  let file_str = Path.to_string file in
  let cmd = format "cd %s && %s -modules %s 2>/dev/null" (Path.to_string cwd) ocamldep file_str in

  Log.debug "[OCAMLDEP] Running for %s: %s" file_str cmd;

  let deps_str =
    let command = Command.make ~args:[ "-c"; cmd ] "sh" in
    match Command.output command with
    | Ok output -> (
        match String.split_on_char '\n' output.Command.stdout with
        | line :: _ -> 
            let trimmed = String.trim line in
            Log.debug "[OCAMLDEP] Result for %s: %s" file_str trimmed;
            trimmed
        | [] ->
            Log.debug "[OCAMLDEP] Empty output for %s" file_str;
            "")
    | Error _err ->
        Log.debug "[OCAMLDEP] Error for %s" file_str;
        ""
  in

  if deps_str = "" then (
    Log.debug "[OCAMLDEP] No deps for %s" file_str;
    [])
  else
    (* Output format: "file.ml: Module1 Module2 Module3" *)
    match String.split_on_char ':' deps_str with
    | [ _; deps_part ] ->
        let deps = String.trim deps_part in
        if deps = "" then (
          Log.debug "[OCAMLDEP] Empty deps part for %s" file_str;
          [])
        else
          let result = String.split_on_char ' ' deps
          |> List.map String.trim
          |> List.map (fun modname ->
              (* Convert string module name to Module_name.t with proper namespace *)
              Model.Module_name.of_string ~namespace:package_namespace modname) in
          Log.debug "[OCAMLDEP] Parsed %d deps for %s: %s" (List.length result) file_str deps;
          result
    | _ ->
        Log.debug "[OCAMLDEP] Failed to parse deps for %s: %s" file_str deps_str;
        []

(** Get dependencies for a single file with optional flags - returns
    Module_name.t list *)
let deps_with_flags ~toolchain ~cwd ~file ~flags ~package_namespace =
  let ocamldep = Path.to_string (Toolchains.ocamldep_path toolchain) in
  let flags_str = Ocamlc.flags_to_string flags |> String.concat " " in
  let file_str = Path.to_string file in
  (* Always include current directory so ocamldep can find .cmi files *)
  let cmd =
    format "cd %s && %s -I . %s -modules %s 2>/dev/null" (Path.to_string cwd) ocamldep flags_str
      file_str
  in

  let deps_str =
    let command = Command.make ~args:[ "-c"; cmd ] "sh" in
    match Command.output command with
    | Ok output -> (
        match String.split_on_char '\n' output.Command.stdout with
        | line :: _ -> String.trim line
        | [] -> "")
    | Error _ -> ""
  in

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
