type file_kind = MLI | ML | Alias

type entry = {
  file : string;
  simple_name : string;
  namespaced : string;
  kind : file_kind;
  is_library_interface : bool;
}

type t = {
  mutable entries : entry list;
  by_simple : (string, entry list) Hashtbl.t;
  by_namespaced : (string, entry list) Hashtbl.t;
}

let create () =
  {
    entries = [];
    by_simple = Hashtbl.create 100;
    by_namespaced = Hashtbl.create 100;
  }

let register registry entry =
  registry.entries <- entry :: registry.entries;

  (* Add to simple name index *)
  let simple_entries =
    try Hashtbl.find registry.by_simple entry.simple_name with Not_found -> []
  in
  Hashtbl.replace registry.by_simple entry.simple_name (entry :: simple_entries);

  (* Add to namespaced index *)
  let ns_entries =
    try Hashtbl.find registry.by_namespaced entry.namespaced
    with Not_found -> []
  in
  Hashtbl.replace registry.by_namespaced entry.namespaced (entry :: ns_entries)

let find_by_simple_name registry name =
  try Hashtbl.find registry.by_simple name with Not_found -> []

let find_by_namespaced registry name =
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
      Printf.printf "  %s -> %s%s%s\n" entry.file entry.namespaced kind_str
        (if entry.is_library_interface then " [library-interface]" else ""))
    (all_entries registry)
