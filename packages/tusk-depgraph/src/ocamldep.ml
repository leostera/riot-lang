open Std

(** OCamldep wrapper for dependency analysis *)

let ocamldep_path =
  match Env.home_dir () with
  | Some home ->
      let p = Path.join home (Path.of_string ".tusk/toolchains/5.3.0/bin/ocamldep" |> Result.expect ~msg:"") in
      Path.to_string p
  | None -> "/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamldep"  (* Fallback *)

(** Run ocamldep to get module dependencies for a file *)
let get_deps ~(cwd: Path.t) ~(file: Path.t) ?(open_modules = []) () =
  let cmd = Command.make ocamldep_path in

  (* Add -I flag for current directory *)
  let cmd = Command.(cmd |> arg "-I" |> arg (Path.to_string cwd)) in

  (* Add -open flags *)
  let cmd = List.fold_left (fun cmd m ->
    Command.(cmd |> arg "-open" |> arg m)
  ) cmd open_modules in

  (* Add -modules flag and file path *)
  let full_path = Path.join cwd file in
  let cmd = Command.(cmd |> arg "-modules" |> arg (Path.to_string full_path)) in
  match Command.output cmd with
  | Ok output ->
      (* Get first line of stdout *)
      (match String.split_on_char '\n' output.stdout with
       | line :: _ when line <> "" -> Some line
       | _ -> None)
  | Error _ -> None

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
let sort_files ~(cwd: Path.t) ~(files: Path.t list) =
  if files = [] then []
  else
    let cmd = Command.make ocamldep_path in

    (* Add -I flag for current directory *)
    let cmd = Command.(cmd |> arg "-I" |> arg (Path.to_string cwd)) in

    (* Add -sort flag *)
    let cmd = Command.(cmd |> arg "-sort") in

    (* Add all files with full paths *)
    let cmd = List.fold_left (fun cmd file ->
      let full_path = Path.join cwd file in
      Command.(cmd |> arg (Path.to_string full_path))
    ) cmd files in

    match Command.output cmd with
    | Ok output ->
        (match String.split_on_char '\n' output.stdout with
         | sorted_str :: _ when sorted_str <> "" ->
             (* ocamldep returns full paths, convert back to Path.t *)
             String.split_on_char ' ' sorted_str
             |> List.filter_map (fun s ->
                if s = "" then None
                else match Path.of_string s with
                  | Ok p -> Some (Path.v (Path.basename p))
                  | Error _ -> None)
         | _ -> files)
    | Error _ -> files (* Return original list if ocamldep fails *)