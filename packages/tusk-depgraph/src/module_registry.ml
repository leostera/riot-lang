open Std

(** Constants for module naming conventions *)
let namespace_separator = "__"

(** Convert namespaced parts to string *)
let namespaced_to_string parts = String.concat namespace_separator parts

(** Convert string to namespaced parts *)
let string_to_namespaced str = String.split_on_char '_' str |> List.filter (fun s -> s <> "")
let path_separator = '/'
let current_dir = "."
let empty_dir = ""
let mli_extension = ".mli"
let ml_extension = ".ml"

type file_kind = MLI | ML | Alias

type entry = {
  file : string;
  simple_name : string;
  namespaced : string list;
  kind : file_kind;
  is_library_interface : bool;
}

type t = {
  mutable entries : entry list;
  by_simple : (string, entry list) Hashtbl.t;
  by_namespaced : (string, entry list) Hashtbl.t;
  package_name : string;
}

let create ~package_name =
  {
    entries = [];
    by_simple = Hashtbl.create 100;
    by_namespaced = Hashtbl.create 100;
    package_name;
  }

(** Convert a file path to a module name, handling subdirectories *)
let module_name_from_path path =
  match Path.of_string path with
  | Error _ -> failwith ("Invalid path: " ^ path)
  | Ok p ->
      let stem_path = Path.remove_extension p in
      let stem = Path.basename stem_path in
      let dir = Path.dirname p |> Path.to_string in

      (* Build module parts from directory structure *)
      let module_parts =
        if dir = current_dir || dir = empty_dir then
          [stem]
        else
          String.split_on_char path_separator dir @ [stem]
      in

      (* Capitalize each part and join with namespace separator *)
      module_parts
      |> List.map String.capitalize_ascii
      |> String.concat namespace_separator

(** Create a namespaced module name *)
let make_namespaced registry module_name =
  [registry.package_name; module_name]

(** Create an entry from a file path *)
let entry_from_file registry file =
  let path = match Path.of_string file with
    | Ok p -> p
    | Error _ -> failwith ("Invalid file path: " ^ file)
  in

  let ext = Path.extension path in
  let kind = match ext with
    | Some ext when ext = mli_extension -> MLI
    | Some ext when ext = ml_extension -> ML
    | _ -> failwith ("Unexpected file extension: " ^ file)
  in

  let stem_path = Path.remove_extension path in
  let simple_name = Path.basename stem_path |> String.capitalize_ascii in
  let module_name = module_name_from_path file in
  let namespaced = make_namespaced registry module_name in

  {
    file;
    simple_name;
    namespaced;
    kind;
    is_library_interface = false;  (* TODO: detect library interfaces *)
  }

let register registry entry =
  registry.entries <- entry :: registry.entries;

  (* Add to simple name index *)
  let simple_entries =
    try Hashtbl.find registry.by_simple entry.simple_name with Not_found -> []
  in
  Hashtbl.replace registry.by_simple entry.simple_name (entry :: simple_entries);

  (* Add to namespaced index *)
  let namespaced_key = namespaced_to_string entry.namespaced in
  let ns_entries =
    try Hashtbl.find registry.by_namespaced namespaced_key
    with Not_found -> []
  in
  Hashtbl.replace registry.by_namespaced namespaced_key (entry :: ns_entries)

let find_by_simple_name registry name =
  try Hashtbl.find registry.by_simple name with Not_found -> []

let find_by_namespaced registry name_parts =
  let name = namespaced_to_string name_parts in
  try Hashtbl.find registry.by_namespaced name with Not_found -> []

let all_entries registry = List.rev registry.entries

let dump registry =
  Printf.printf "Module Registry (%d entries):\n" (List.length registry.entries);
  List.iter
    (fun entry ->
      let kind_str =
        match entry.kind with
        | MLI -> " [.mli]"
        | ML -> " [.ml]"
        | Alias -> " [alias]"
      in
      Printf.printf "  %s -> %s%s%s\n" entry.file (namespaced_to_string entry.namespaced) kind_str
        (if entry.is_library_interface then " [library-interface]" else ""))
    (all_entries registry)
