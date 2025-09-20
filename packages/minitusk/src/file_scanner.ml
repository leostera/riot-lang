(** File scanner module for building directory trees *)

type file = { path : string; name : string; ext : string }

type dir = { path : string; name : string; children : file_tree list }
and file_tree = File of file | Dir of dir

let path t = match t with File f -> f.path | Dir d -> d.path

(** Walk a directory tree and build a file_tree structure *)
let rec walk ~root =
  let name = Filename.basename root in

  if not (Sys.file_exists root) then
    failwith (Printf.sprintf "Path does not exist: %s" root)
  else if Sys.is_directory root then
    (* Get all entries in the directory *)
    let entries =
      Sys.readdir root |> Array.to_list
      |> List.filter (fun entry ->
          (* Filter out hidden files and current/parent dir *)
          entry <> "." && entry <> ".."
          && not (String.starts_with ~prefix:"." entry))
    in

    (* Build full paths and recursively process *)
    let children =
      List.map
        (fun entry ->
          let full_path = Filename.concat root entry in
          walk ~root:full_path)
        entries
    in

    Dir { path = root; name; children }
  else
    (* It's a file *)
    let ext = Filename.extension name in
    File { path = root; name; ext }

(** Pretty print a file tree for debugging *)
let rec print_tree ?(indent = 0) tree =
  let prefix = String.make (indent * 2) ' ' in
  match tree with
  | File { name; _ } -> Printf.printf "%s- %s\n" prefix name
  | Dir { name; children; _ } ->
      Printf.printf "%s+ %s/\n" prefix name;
      List.iter (print_tree ~indent:(indent + 1)) children

(** Flatten tree to a list of file paths *)
let rec flatten_to_paths tree =
  match tree with
  | File { path; _ } -> [ path ]
  | Dir { children; _ } -> List.concat_map flatten_to_paths children

(** Get only directories (no files) *)
let rec get_directories_only tree =
  match tree with
  | File _ -> None
  | Dir { path; name; children } ->
      let dir_children = List.filter_map get_directories_only children in
      Some (Dir { path; name; children = dir_children })
