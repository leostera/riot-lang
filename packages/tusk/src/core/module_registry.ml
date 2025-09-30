(** Module registry for managing module name resolution *)

type entry = {
  file : string; (* e.g., "build_node.ml" *)
  simple_name : string; (* e.g., "Build_node" *)
  namespaced : string; (* e.g., "Tusk__Core__Build_node" *)
  submodule_names : string list; (* e.g., ["Core__Build_node"] *)
  is_alias : bool; (* Whether this is an alias module *)
}

type t = {
  mutable entries : entry list;
  by_file : (string, entry) Hashtbl.t;
  by_simple_name : (string, entry) Hashtbl.t;
  by_namespaced : (string, entry) Hashtbl.t;
  by_any_name : (string, entry) Hashtbl.t; (* Includes all possible names *)
}

let create () =
  {
    entries = [];
    by_file = Hashtbl.create 100;
    by_simple_name = Hashtbl.create 100;
    by_namespaced = Hashtbl.create 100;
    by_any_name = Hashtbl.create 100;
  }

let register registry entry =
  (* Add to entries list *)
  registry.entries <- entry :: registry.entries;

  (* Index by file *)
  Hashtbl.add registry.by_file entry.file entry;

  (* Index by simple name *)
  Hashtbl.add registry.by_simple_name entry.simple_name entry;

  (* Index by namespaced name *)
  Hashtbl.add registry.by_namespaced entry.namespaced entry;

  (* Index by all names in the any_name table *)
  Hashtbl.add registry.by_any_name entry.file entry;
  Hashtbl.add registry.by_any_name entry.simple_name entry;
  Hashtbl.add registry.by_any_name entry.namespaced entry;

  (* Also index by submodule names *)
  List.iter
    (fun name -> Hashtbl.add registry.by_any_name name entry)
    entry.submodule_names

let find_by_file registry file = Hashtbl.find_opt registry.by_file file

let find_by_simple_name registry name =
  Hashtbl.find_opt registry.by_simple_name name

let find_by_namespaced registry name =
  Hashtbl.find_opt registry.by_namespaced name

let find_by_name registry name = Hashtbl.find_opt registry.by_any_name name
let all_entries registry = registry.entries
let entry_simple_name entry = entry.simple_name
let entry_namespaced entry = entry.namespaced
let entry_file entry = entry.file
let entry_is_alias entry = entry.is_alias
