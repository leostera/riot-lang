(** Module namespacing utilities for tusk build system *)

(** Convert snake_case to CamelCase *)
let snake_to_camel s =
  let parts = String.split_on_char '_' s in
  String.concat ""
    (List.map (fun part ->
      if String.length part > 0 then
        String.capitalize_ascii part
      else
        part) parts)

(** Convert a file path to a namespaced module name
    Examples:
    - "hello_world.ml" -> "HelloWorld"
    - "a/b/c.ml" -> "A__B__C"
    - "a/hello_world.ml" -> "A__HelloWorld"
    - "lib/parser_utils.ml" -> "Lib__ParserUtils"
*)
let path_to_module_name ~package_name path =
  (* Remove .ml or .mli extension *)
  let path_without_ext =
    if Filename.check_suffix path ".ml" then
      Filename.chop_suffix path ".ml"
    else if Filename.check_suffix path ".mli" then
      Filename.chop_suffix path ".mli"
    else
      path
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
  (* Get just the filename without directory *)
  let basename = Filename.basename file_path in
  
  (* Remove extension *)
  let name_without_ext =
    if Filename.check_suffix basename ".ml" then
      Filename.chop_suffix basename ".ml"
    else if Filename.check_suffix basename ".mli" then
      Filename.chop_suffix basename ".mli"
    else
      basename
  in
  
  (* Check if this is the main package file *)
  if name_without_ext = package_name then
    String.capitalize_ascii package_name
  else if name_without_ext = "main" then
    (* main.ml doesn't get namespaced *)
    "Main"
  else
    (* Get the relative path from src directory *)
    let dir = Filename.dirname file_path in
    if dir = "." || dir = "" then
      (* File is in root of src - just convert name *)
      String.capitalize_ascii package_name ^ "__" ^ snake_to_camel name_without_ext
    else
      (* File is in subdirectory - include path in namespace *)
      path_to_module_name ~package_name file_path

(** Generate a module alias file that re-exports all modules in a package *)
let generate_package_module ~package_name ~modules =
  let buffer = Buffer.create 1024 in
  
  (* Add header comment *)
  Buffer.add_string buffer
    (Printf.sprintf "(* Auto-generated module for package %s *)\n" package_name);
  Buffer.add_string buffer "(* This module re-exports all modules in the package *)\n\n";
  
  (* Generate module aliases for each module *)
  List.iter (fun (original_name, transformed_name) ->
    if transformed_name <> String.capitalize_ascii package_name then (
      (* Extract the module name without package prefix *)
      let module_alias =
        if String.starts_with ~prefix:(String.capitalize_ascii package_name ^ "__") transformed_name then
          let prefix_len = String.length package_name + 2 in
          String.sub transformed_name prefix_len (String.length transformed_name - prefix_len)
        else
          transformed_name
      in
      Buffer.add_string buffer
        (Printf.sprintf "module %s = %s\n" module_alias transformed_name)
    )
  ) modules;
  
  Buffer.contents buffer

(** Get the flattened module name for compilation 
    Example: "a/b/hello_world.ml" -> "pkg__a__b__hello_world.ml" *)
let get_flat_filename ~package_name file_path =
  let dir = Filename.dirname file_path in
  let basename = Filename.basename file_path in
  
  (* Remove extension *)
  let (name_without_ext, ext) =
    if Filename.check_suffix basename ".ml" then
      (Filename.chop_suffix basename ".ml", ".ml")
    else if Filename.check_suffix basename ".mli" then
      (Filename.chop_suffix basename ".mli", ".mli")
    else if Filename.check_suffix basename ".c" then
      (Filename.chop_suffix basename ".c", ".c")
    else
      (basename, "")
  in
  
  (* Special case for main.ml and package.ml *)
  if name_without_ext = "main" || name_without_ext = package_name then
    basename
  else if dir = "." || dir = "" then
    (* File in root - prefix with package name *)
    Printf.sprintf "%s__%s%s" package_name name_without_ext ext
  else
    (* File in subdirectory - include full path *)
    let path_parts = String.split_on_char '/' dir in
    let full_prefix = String.concat "__" (package_name :: path_parts) in
    Printf.sprintf "%s__%s%s" full_prefix name_without_ext ext