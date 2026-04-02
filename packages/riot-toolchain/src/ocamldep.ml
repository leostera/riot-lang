open Std
open Std.Collections
open Riot_model

(** OCamldep wrapper - handles dependency analysis *)
type t = Path.t

let make = fun path -> path

let path = fun t -> t

let sort = fun t ~cwd ~files ->
  if files = [] then
    []
  else
    let ocamldep = Path.to_string t in
    let files_str = String.concat " " (List.map Path.to_string files) in
    let cmd = "cd " ^ Path.to_string cwd ^ " && " ^ ocamldep ^ " -sort " ^ files_str ^ " 2>/dev/null" in
    Log.trace @@ "  $ " ^ cmd;
    let sorted_str =
      let command = Command.make ~args:[ "-c"; cmd ] "sh" in
      match Command.output command with
      | Ok output -> (
          match String.split_on_char '\n' output.Command.stdout with
          | line :: _ -> String.trim line
          | [] -> ""
        )
      | Error _ -> ""
    in
    Log.trace @@ "  [ocamldep] Result: " ^ sorted_str;
    if sorted_str = "" then
      files
      (* Return original list if ocamldep fails *)
    else
      let sorted_basenames = String.split_on_char ' ' sorted_str in
      (* Filter out empty strings and return files in dependency order *)
      List.filter_map
        (fun s ->
          if s = "" then
            None
          else
            Some (Path.v s))
        sorted_basenames

let deps = fun t ~cwd ~file ~package_namespace ->
  let ocamldep = Path.to_string t in
  let file_str = Path.to_string file in
  let cmd = "cd " ^ Path.to_string cwd ^ " && " ^ ocamldep ^ " -modules " ^ file_str ^ " 2>/dev/null" in
  Log.trace @@ "[OCAMLDEP] Running for " ^ file_str ^ ": " ^ cmd;
  let deps_str =
    let command = Command.make ~args:[ "-c"; cmd ] "sh" in
    match Command.output command with
    | Ok output -> (
        match String.split_on_char '\n' output.Command.stdout with
        | line :: _ ->
            let trimmed = String.trim line in
            Log.trace ("[OCAMLDEP] Result for " ^ file_str ^ ": " ^ trimmed);
            trimmed
        | [] ->
            Log.trace ("[OCAMLDEP] Empty output for " ^ file_str);
            ""
      )
    | Error _err ->
        Log.trace ("[OCAMLDEP] Error for " ^ file_str);
        ""
  in
  if deps_str = "" then
    (
      Log.trace ("[OCAMLDEP] No deps for " ^ file_str);
      []
    )
  else
    (* Output format: "file.ml: Module1 Module2 Module3" *)
    match String.split_on_char ':' deps_str with
    | [_;deps_part] ->
        let deps = String.trim deps_part in
        if deps = "" then
          (
            Log.trace ("[OCAMLDEP] Empty deps part for " ^ file_str);
            []
          )
        else
          let result = String.split_on_char ' ' deps
          |> List.map String.trim
          |> List.map (fun modname -> Module_name.of_string ~namespace:package_namespace modname) in
          Log.trace
            ("[OCAMLDEP] Parsed "
            ^ Int.to_string (List.length result)
            ^ " deps for "
            ^ file_str
            ^ ": "
            ^ deps);
          result
    | _ ->
        Log.trace ("[OCAMLDEP] Failed to parse deps for " ^ file_str ^ ": " ^ deps_str);
        []

(** Get dependencies for a single file with optional flags - returns
    Module_name.t list *)
let deps_with_flags = fun t ~cwd ~file ~flags ~package_namespace ->
  let ocamldep = Path.to_string t in
  let flags_str = Ocamlc.flags_to_string flags |> String.concat " " in
  let file_str = Path.to_string file in
  (* Always include current directory so ocamldep can find .cmi files *)
  let cmd =
    "cd "
    ^ Path.to_string cwd
    ^ " && "
    ^ ocamldep
    ^ " -I . "
    ^ flags_str
    ^ " -modules "
    ^ file_str
    ^ " 2>/dev/null"
  in
  let deps_str =
    let command = Command.make ~args:[ "-c"; cmd ] "sh" in
    match Command.output command with
    | Ok output -> (
        match String.split_on_char '\n' output.Command.stdout with
        | line :: _ -> String.trim line
        | [] -> ""
      )
    | Error _ -> ""
  in
  if deps_str = "" then
    []
  else
    (* Output format: "file.ml: Module1 Module2 Module3" *)
    match String.split_on_char ':' deps_str with
    | [_;deps_part] ->
        let deps = String.trim deps_part in
        if deps = "" then
          []
        else
          String.split_on_char ' ' deps
          |> List.map String.trim
          |> List.map (fun modname -> Module_name.of_string ~namespace:package_namespace modname)
    | _ -> []

(** Get dependencies for multiple files in one ocamldep call - returns (file,
    deps) list *)
let batch_deps = fun t ~cwd ~files ~package_namespace ->
  if files = [] then
    []
  else
    let ocamldep = Path.to_string t in
    let files_str = String.concat " " (List.map Path.to_string files) in
    let cmd = "cd " ^ Path.to_string cwd ^ " && " ^ ocamldep ^ " -modules " ^ files_str in
    Log.trace ("[OCAMLDEP] Batch running for " ^ Int.to_string (List.length files) ^ " files");
    Log.debug ("[OCAMLDEP] CMD: " ^ cmd);
    let output =
      let command = Command.make ~args:[ "-c"; cmd ] "sh" in
      match Command.output command with
      | Ok output ->
          Log.debug ("[OCAMLDEP] OUTPUT: '" ^ output.Command.stdout ^ "'");
          output.Command.stdout
      | Error (Command.SystemError msg) ->
          Log.debug ("[OCAMLDEP] ERROR: " ^ msg);
          ""
    in
    if output = "" then
      List.map (fun file -> (file, [])) files
    else
      (* Parse output - each line is "file.ml: Module1 Module2 Module3" *)
      let lines = String.split_on_char '\n' output in
      List.filter_map
        (fun line ->
          let trimmed = String.trim line in
          if trimmed = "" then
            None
          else
            match String.split_on_char ':' trimmed with
            | [file_part;deps_part] ->
                let file = Path.v (String.trim file_part) in
                let deps = String.trim deps_part in
                let dep_list =
                  if deps = "" then
                    []
                  else
                    String.split_on_char ' ' deps
                    |> List.map String.trim
                    |> List.filter (fun s -> s != "")
                    |> List.map
                      (fun modname -> Module_name.of_string ~namespace:package_namespace modname)
                in
                Some (file, dep_list)
            | _ -> None)
        lines

(** Get all module dependencies (for building .merlin files) *)
let all_deps = fun t ~cwd ~files ~package_namespace ->
  List.map
    (fun file ->
      let deps = deps t ~cwd ~file ~package_namespace in
      (file, deps))
    files
