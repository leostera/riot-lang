(* Package registry for cross-package module discovery *)

type package_info = {
  name: string;
  path: string;
  modules: (string * string) list; (* module_name -> file_path *)
  dependencies: string list;
}

type t = {
  packages: (string, package_info) Hashtbl.t;
  module_index: (string, string list) Hashtbl.t; (* module_name -> [package_names] *)
}

let create () = {
  packages = Hashtbl.create 32;
  module_index = Hashtbl.create 128;
}

let register_package registry pkg_info =
  Hashtbl.add registry.packages pkg_info.name pkg_info;
  
  (* Index all modules from this package *)
  List.iter (fun (module_name, _file_path) ->
    let packages = 
      try Hashtbl.find registry.module_index module_name
      with Not_found -> []
    in
    Hashtbl.replace registry.module_index module_name (pkg_info.name :: packages)
  ) pkg_info.modules

let find_module registry ~current_package ~module_name =
  (* First check if module is in current package *)
  match Hashtbl.find_opt registry.packages current_package with
  | Some pkg_info ->
      (match List.assoc_opt module_name pkg_info.modules with
      | Some file_path -> Ok (current_package, file_path)
      | None ->
          (* Check dependencies *)
          let rec check_deps deps =
            match deps with
            | [] -> 
                (* Module not found in current package or its dependencies *)
                (match Hashtbl.find_opt registry.module_index module_name with
                | Some packages ->
                    Error (Printf.sprintf 
                      "Module '%s' not found in package '%s' or its dependencies.\nFound in packages: %s\nConsider adding one of these as a dependency."
                      module_name current_package (String.concat ", " packages))
                | None ->
                    Error (Printf.sprintf "Module '%s' not found in any registered package" module_name))
            | dep :: rest ->
                match Hashtbl.find_opt registry.packages dep with
                | Some dep_pkg ->
                    (match List.assoc_opt module_name dep_pkg.modules with
                    | Some file_path -> Ok (dep, file_path)
                    | None -> check_deps rest)
                | None ->
                    Error (Printf.sprintf "Dependency package '%s' not found in registry" dep)
          in
          check_deps pkg_info.dependencies)
  | None ->
      Error (Printf.sprintf "Package '%s' not found in registry" current_package)

let list_modules_in_package registry package_name =
  match Hashtbl.find_opt registry.packages package_name with
  | Some pkg_info -> Ok (List.map fst pkg_info.modules)
  | None -> Error (Printf.sprintf "Package '%s' not found" package_name)

let get_package_dependencies registry package_name =
  match Hashtbl.find_opt registry.packages package_name with
  | Some pkg_info -> Ok pkg_info.dependencies
  | None -> Error (Printf.sprintf "Package '%s' not found" package_name)

(* Build a topological order of packages based on dependencies *)
let topological_sort registry =
  let visited = Hashtbl.create 32 in
  let rec_stack = Hashtbl.create 32 in
  let result = ref [] in
  
  let rec visit pkg_name =
    if Hashtbl.mem rec_stack pkg_name then
      Error (Printf.sprintf "Circular dependency detected involving package '%s'" pkg_name)
    else if not (Hashtbl.mem visited pkg_name) then
      match Hashtbl.find_opt registry.packages pkg_name with
      | Some pkg_info ->
          Hashtbl.add rec_stack pkg_name ();
          let deps_result = 
            List.fold_left (fun acc dep ->
              match acc with
              | Error _ as err -> err
              | Ok () -> visit dep
            ) (Ok ()) pkg_info.dependencies
          in
          Hashtbl.remove rec_stack pkg_name;
          (match deps_result with
          | Error _ as err -> err
          | Ok () ->
              Hashtbl.add visited pkg_name ();
              result := pkg_name :: !result;
              Ok ())
      | None ->
          Error (Printf.sprintf "Package '%s' not found during topological sort" pkg_name)
    else
      Ok ()
  in
  
  let all_packages = Hashtbl.fold (fun name _ acc -> name :: acc) registry.packages [] in
  match List.fold_left (fun acc pkg ->
    match acc with
    | Error _ as err -> err
    | Ok () -> visit pkg
  ) (Ok ()) all_packages with
  | Ok () -> Ok !result
  | Error _ as err -> err