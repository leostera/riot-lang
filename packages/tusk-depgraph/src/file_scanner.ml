open Std

(** Constants *)
let path_separator = "/"
let ml_extension = ".ml"
let mli_extension = ".mli"
let c_extension = ".c"
let h_extension = ".h"

type file_info = {
  path : string;  (* Relative path from root *)
  extension : string option;  (* File extension if any *)
  is_directory : bool;
}

type scan_result = {
  files : file_info list;
  directories : string list;
}

let empty_result = { files = []; directories = [] }

let get_extension filename =
  match Path.of_string filename with
  | Error _ -> None
  | Ok path -> Path.extension path

let rec scan_directory ~root =
  let root_path = match Path.of_string root with
    | Ok p -> p
    | Error _ -> failwith ("Invalid root path: " ^ root)
  in

  let rec scan_rec base_path acc =
    match Fs.read_dir base_path with
    | Error e ->
        Printf.eprintf "Warning: Could not read directory %s: %s\n"
          (Path.to_string base_path)
          (match e with Fs.SystemError s -> s);
        acc
    | Ok dir_iter ->
        let rec process_entries acc =
          match Fs.ReadDir.next dir_iter with
          | None ->
              (* Close the iterator when done *)
              let _ = Fs.ReadDir.close dir_iter in
              acc
          | Some entry ->
              (* Build full path *)
              let entry_path = Path.join base_path entry in

              (* Make path relative to root *)
              let rel_path =
                let full = Path.to_string entry_path in
                let root_str = Path.to_string root_path ^ path_separator in
                if String.starts_with ~prefix:root_str full then
                  String.sub full (String.length root_str)
                    (String.length full - String.length root_str)
                else
                  Path.to_string entry_path
              in

              (* Check if directory or file *)
              match Fs.is_directory entry_path with
              | Error _ -> process_entries acc
              | Ok true ->
                  (* Add directory to list and recurse *)
                  let new_directories = rel_path :: acc.directories in
                  let acc' = { files = acc.files; directories = new_directories } in
                  let acc'' = scan_rec entry_path acc' in
                  process_entries acc''
              | Ok false ->
                  (* Add file to list *)
                  let file_info = {
                    path = rel_path;
                    extension = get_extension rel_path;
                    is_directory = false;
                  } in
                  let new_files = file_info :: acc.files in
                  let acc' = { files = new_files; directories = acc.directories } in
                  process_entries acc'
        in
        process_entries acc
  in

  let result = scan_rec root_path empty_result in
  {
    files = List.rev result.files;
    directories = List.rev result.directories
  }

let scan ~root =
  Printf.printf "Scanning directory tree from: %s\n" root;
  let result = scan_directory ~root in
  Printf.printf "Found %d files and %d directories\n"
    (List.length result.files)
    (List.length result.directories);
  result

(** Filter helpers *)

let filter_by_extension ext files =
  List.filter (fun f ->
    match f.extension with
    | None -> false
    | Some e -> e = ext
  ) files

let filter_by_extensions exts files =
  List.filter (fun f ->
    match f.extension with
    | None -> false
    | Some e -> List.mem e exts
  ) files

let ocaml_source_files result =
  filter_by_extensions [ml_extension; mli_extension] result.files

let c_source_files result =
  filter_by_extensions [c_extension; h_extension] result.files

let all_files result =
  result.files

let all_file_paths result =
  List.map (fun f -> f.path) result.files