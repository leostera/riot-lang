(** Simple ocamldep wrapper for dependency analysis *)

let ocamldep_path = "/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamldep"

(** Run ocamldep to get module dependencies for a file *)
let get_deps ~cwd ~file =
  let cmd =
    Printf.sprintf "cd %s && %s -modules %s 2>/dev/null" cwd ocamldep_path file
  in
  let ic = Unix.open_process_in cmd in
  let result =
    try
      let line = input_line ic in
      Some line
    with End_of_file -> None
  in
  ignore (Unix.close_process_in ic);
  result

(** Parse ocamldep output to extract module names *)
let parse_deps line =
  (* Format: "file.ml: Module1 Module2 Module3" *)
  match String.split_on_char ':' line with
  | [ _file; deps_str ] ->
      let deps = String.trim deps_str in
      if deps = "" then []
      else String.split_on_char ' ' deps |> List.map String.trim
  | _ -> []

(** Sort files in dependency order *)
let sort_files ~cwd ~files =
  if files = [] then []
  else
    let files_str = String.concat " " files in
    let cmd =
      Printf.sprintf "cd %s && %s -sort %s 2>/dev/null" cwd ocamldep_path
        files_str
    in

    let ic = Unix.open_process_in cmd in
    let sorted_str = try input_line ic with End_of_file -> "" in
    ignore (Unix.close_process_in ic);

    if sorted_str = "" then files (* Return original list if ocamldep fails *)
    else
      let sorted_basenames = String.split_on_char ' ' sorted_str in
      (* Filter out empty strings and return files in dependency order *)
      List.filter (fun s -> s <> "") sorted_basenames
