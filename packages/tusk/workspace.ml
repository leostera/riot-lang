type source_file = {
  path: string;
  package: string;
  module_name: string;
  namespaced_name: string;  (* Ox-style namespaced module name *)
  file_type: [`ML | `MLI];
}

(* Ox-style module namespacing *)
let to_class_case s =
  let parts = String.split_on_char '_' s |> List.map (String.split_on_char '-') |> List.flatten in
  String.concat "" (List.map String.capitalize_ascii parts)

let make_namespaced_name ~package_root ~package_name ~source_path =
  let basename = Filename.basename source_path in
  let module_name = 
    if String.ends_with ~suffix:".ml" basename then
      Filename.chop_suffix basename ".ml"
    else if String.ends_with ~suffix:".mli" basename then
      Filename.chop_suffix basename ".mli"
    else basename
  in
  let module_name = String.capitalize_ascii module_name in
  
  (* Get directory path relative to package root *)
  let dir_path = Filename.dirname source_path in
  let relative_dir = 
    if String.starts_with ~prefix:package_root dir_path then
      let len = String.length package_root in
      let rest = String.sub dir_path (len + 1) (String.length dir_path - len - 1) in
      if rest = "" then "" else rest
    else ""
  in
  
  (* Build namespace: PackageName__Dir__Module *)
  let package_ns = to_class_case package_name in
  let dir_ns = if relative_dir = "" then "" else "__" ^ (to_class_case (String.map (function '/' -> '_' | c -> c) relative_dir)) in
  let namespaced_name = package_ns ^ dir_ns ^ "__" ^ module_name in
  
  (module_name, namespaced_name)

let get_module_name_from_file file_path =
  let basename = Filename.basename file_path in
  let name_part = 
    if String.ends_with ~suffix:".ml" basename then
      Filename.chop_suffix basename ".ml"
    else if String.ends_with ~suffix:".mli" basename then
      Filename.chop_suffix basename ".mli"
    else basename
  in
  String.capitalize_ascii name_part

let find_files dir pattern =
  try
    let files = Sys.readdir dir in
    Array.to_list files
    |> List.filter (fun f -> 
        let len = String.length pattern in
        String.length f >= len && 
        String.sub f (String.length f - len) len = pattern)
    |> List.map (fun f -> Filename.concat dir f)
  with _ -> []

let scan_package_for_files package_name package_path =
  let ml_files = find_files package_path ".ml" in
  let mli_files = find_files package_path ".mli" in
  
  let ml_sources = List.map (fun path -> 
    let (module_name, namespaced_name) = make_namespaced_name ~package_root:package_path ~package_name ~source_path:path in
    {
      path;
      package = package_name;
      module_name;
      namespaced_name;
      file_type = `ML;
    }) ml_files in
  
  let mli_sources = List.map (fun path -> 
    let (module_name, namespaced_name) = make_namespaced_name ~package_root:package_path ~package_name ~source_path:path in
    {
      path;
      package = package_name;
      module_name;
      namespaced_name;
      file_type = `MLI;
    }) mli_files in
  
  ml_sources @ mli_sources

let discover_workspace_files workspace_toml_path =
  (* Parse workspace config to get members *)
  let workspace_config = Workspace_config.parse_workspace_toml workspace_toml_path in
  let root_dir = Filename.dirname workspace_toml_path in
  
  (* Scan each member package *)
  List.fold_left (fun acc member_path ->
    let full_path = Filename.concat root_dir member_path in
    let package_name = Filename.basename member_path in
    if Sys.file_exists full_path && Sys.is_directory full_path then
      let package_files = scan_package_for_files package_name full_path in
      package_files @ acc
    else
      acc
  ) [] workspace_config.Workspace_config.members