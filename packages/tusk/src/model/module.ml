(* OCaml module representation *)

type file_kind =
  | ML_file of { path : string }
  | MLI_file of { path : string }
  | Generated of { path : string; contents : string }

type module_kind = MLI | ML | Aliases | LibraryInterface

and t = {
  module_name : string;
  namespaced_name : string;
  path : string;
  kind : [ `implementation | `interface ];
  file : file_kind;
  module_kind : module_kind;
}

let create ~package_name ~path =
  let path_obj = Path.v path in
  let basename = Path.remove_extension path_obj |> Path.basename in
  let module_name = String.capitalize_ascii basename in
  let namespace = Namespace.of_list [ String.capitalize_ascii package_name ] in
  let namespaced_name =
    Namespace.append namespace module_name |> Namespace.to_string
  in
  let kind =
    if String.ends_with ~suffix:".mli" path then `interface else `implementation
  in
  let file =
    if kind = `interface then MLI_file { path } else ML_file { path }
  in
  let module_kind = if kind = `interface then MLI else ML in
  { module_name; namespaced_name; path; kind; file; module_kind }

let module_name t = t.module_name
let namespaced_name t = t.namespaced_name
let path t = t.path
let cmi t = t.namespaced_name ^ ".cmi"
let cmo t = t.namespaced_name ^ ".cmo"
let eq a b = a.namespaced_name = b.namespaced_name
let kind t = t.kind
let is_aliases t = t.module_kind = Aliases

let dependencies t package =
  (* This is a simplified version - in reality we'd use ocamldep *)
  []

let make_alias_module package aliases =
  let package_name = package.Workspace.name in
  let path = "__aliases.ml" in
  let namespace = Namespace.of_string package_name in
  let namespaced_name =
    Namespace.append namespace "Aliases" |> Namespace.to_string
  in
  let contents =
    (* Generate alias content *)
    String.concat "\n"
      (List.map
         (fun alias ->
           Printf.sprintf "module %s = %s__%s" alias package_name alias)
         aliases)
  in
  {
    module_name = "__Aliases";
    namespaced_name;
    path;
    kind = `implementation;
    file = Generated { path; contents };
    module_kind = Aliases;
  }

let make_library_interface package lib_name children aliases ~exists =
  let package_name = package.Workspace.name in
  let path = lib_name ^ ".ml" in
  let namespace = Namespace.of_string package_name in
  let namespaced_name =
    Namespace.append namespace lib_name |> Namespace.to_string
  in
  let contents =
    (* Generate library interface content *)
    ""
  in
  {
    module_name = lib_name;
    namespaced_name;
    path;
    kind = `implementation;
    file = (if exists then ML_file { path } else Generated { path; contents });
    module_kind = LibraryInterface;
  }
