(** Module namespacing utilities for tusk build system *)

(** Convert snake_case to CamelCase *)
let snake_to_camel s =
  let parts = String.split_on_char '_' s in
  String.concat ""
    (List.map
       (fun part ->
         if String.length part > 0 then String.capitalize_ascii part else part)
       parts)

(** Convert a file path to a namespaced module name Examples:
    - "hello_world.ml" -> "HelloWorld"
    - "a/b/c.ml" -> "A__B__C"
    - "a/hello_world.ml" -> "A__HelloWorld"
    - "lib/parser_utils.ml" -> "Lib__ParserUtils" *)
let path_to_module_name ~package_name path =
  (* Remove .ml or .mli extension *)
  let path_without_ext =
    if Filename.check_suffix path ".ml" then Filename.chop_suffix path ".ml"
    else if Filename.check_suffix path ".mli" then
      Filename.chop_suffix path ".mli"
    else path
  in

  (* Split by directory separator *)
  let parts = String.split_on_char '/' path_without_ext in

  (* Convert each part from snake_case to CamelCase *)
  let camel_parts = List.map snake_to_camel parts in

  (* Join with __ for OCaml module namespacing *)
  let module_name = String.concat "__" camel_parts in

  (* Prefix with package name *)
  String.capitalize_ascii package_name ^ "__" ^ module_name

(** Get the transformed module name for a source file *)
let get_module_name ~package_name file_path =
  (* Enable namespacing - prefix with package name *)
  let basename = Filename.basename file_path in

  (* Remove extension *)
  let name_without_ext =
    if Filename.check_suffix basename ".ml" then
      Filename.chop_suffix basename ".ml"
    else if Filename.check_suffix basename ".mli" then
      Filename.chop_suffix basename ".mli"
    else basename
  in

  (* Replace hyphens with underscores in package name to make valid module name *)
  let safe_package_name =
    String.map (fun c -> if c = '-' then '_' else c) package_name
  in

  (* Check if this is the main package module - if so, don't namespace it *)
  if name_without_ext = safe_package_name then
    (* This is the package's main module, don't namespace it *)
    String.capitalize_ascii name_without_ext
  else
    (* Regular module, add namespace prefix *)
    String.capitalize_ascii safe_package_name
    ^ "__"
    ^ String.capitalize_ascii name_without_ext

(** Get the transformed module name with folder-based namespacing Examples:
    - src/utils.ml -> Package__Utils
    - src/cli/build.ml -> Package__Cli__Build
    - src/cli/cli.ml -> Package__Cli (folder interface module) *)
let get_module_name_with_folders ~package_name ~namespace file_path =
  let basename = Filename.basename file_path in

  (* Remove extension *)
  let name_without_ext =
    if Filename.check_suffix basename ".ml" then
      Filename.chop_suffix basename ".ml"
    else if Filename.check_suffix basename ".mli" then
      Filename.chop_suffix basename ".mli"
    else basename
  in

  (* Replace hyphens with underscores in package name *)
  let safe_package_name =
    String.map (fun c -> if c = '-' then '_' else c) package_name
  in

  (* Build the full namespaced name based on folder hierarchy *)
  match namespace with
  | [] ->
      (* No folders - use existing logic *)
      if name_without_ext = safe_package_name then
        String.capitalize_ascii name_without_ext
      else
        String.capitalize_ascii safe_package_name
        ^ "__"
        ^ String.capitalize_ascii name_without_ext
  | folders ->
      (* Has folders - build hierarchical namespace *)
      let folder_parts = List.map String.capitalize_ascii folders in

      (* Check if this is a folder interface module (e.g., cli/cli.ml) *)
      let is_folder_interface =
        match List.rev folders with
        | last_folder :: _ -> last_folder = name_without_ext
        | [] -> false
      in

      if is_folder_interface then
        (* This is a folder interface module - name it after the folder *)
        String.capitalize_ascii safe_package_name
        ^ "__"
        ^ String.concat "__" folder_parts
      else
        (* Regular module within a folder *)
        String.capitalize_ascii safe_package_name
        ^ "__"
        ^ String.concat "__" folder_parts
        ^ "__"
        ^ String.capitalize_ascii name_without_ext

(** Generate a module alias file that re-exports all modules in a package *)
let generate_package_module ~package_name ~modules =
  let buffer = Buffer.create 1024 in

  (* Add header comment *)
  Buffer.add_string buffer
    (Printf.sprintf "(* Auto-generated module for package %s *)\n" package_name);
  Buffer.add_string buffer
    "(* This module re-exports all modules in the package *)\n\n";

  (* Generate module aliases for each module *)
  List.iter
    (fun (original_name, transformed_name) ->
      if transformed_name <> String.capitalize_ascii package_name then
        (* Extract the module name without package prefix *)
        let module_alias =
          if
            String.starts_with
              ~prefix:(String.capitalize_ascii package_name ^ "__")
              transformed_name
          then
            let prefix_len = String.length package_name + 2 in
            String.sub transformed_name prefix_len
              (String.length transformed_name - prefix_len)
          else transformed_name
        in
        Buffer.add_string buffer
          (Printf.sprintf "module %s = %s\n" module_alias transformed_name))
    modules;

  Buffer.contents buffer

(** Get the flattened module name for compilation Example: "a/b/hello_world.ml"
    -> "pkg__a__b__hello_world.ml" *)
let get_flat_filename ~package_name file_path =
  (* Enable namespacing - return namespaced filename *)
  let basename = Filename.basename file_path in

  (* Remove extension to get module name *)
  let name_without_ext =
    if Filename.check_suffix basename ".ml" then
      Filename.chop_suffix basename ".ml"
    else if Filename.check_suffix basename ".mli" then
      Filename.chop_suffix basename ".mli"
    else basename
  in

  (* Add back the extension with namespaced name *)
  let ext = Filename.extension basename in
  String.capitalize_ascii package_name
  ^ "__"
  ^ String.capitalize_ascii name_without_ext
  ^ ext
