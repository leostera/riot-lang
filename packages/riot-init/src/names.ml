open Std
open Riot_model

let validate_name = fun name ->
  match Package.validate_name name with
  | Ok package_name -> Ok package_name
  | Error err -> Error (Failure (Package_name.error_message err))

let validate_workspace_name = fun name ->
  if String.length name = 0 then
    Error (Failure "Workspace name cannot be empty")
  else
    Ok name

let starter_package_name = fun workspace_name ->
  String.map
    ~fn:(fun c ->
      if c = '.' then
        '-'
      else
        c)
    workspace_name

let package_name_to_module_name = fun name ->
  Module_name.from_string name
  |> Module_name.to_string

let module_name_to_test_file_stem = fun module_name -> String.lowercase_ascii module_name ^ "_tests"

let module_name_to_file_stem = fun module_name -> String.lowercase_ascii module_name

let package_module_name = fun name ->
  String.split ~by:"-" name
  |> List.map ~fn:String.capitalize_ascii
  |> String.concat ""
