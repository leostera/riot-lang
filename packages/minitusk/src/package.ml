type t = { name : string; path : string; deps : string list }

let read path =
  let toml_path = Filename.concat path "tusk.toml" in
  if Sys.file_exists toml_path then
    let lines = Toml.parse_file toml_path in
    (* Find lines after [package] section *)
    let rec find_package_section = function
      | [] -> []
      | line :: rest ->
          if line = "[package]" then rest else find_package_section rest
    in
    let package_lines = find_package_section lines in
    let name =
      match Toml.get_string_value package_lines "name" with
      | Some n -> n
      | None -> Filename.basename path
    in
    { name; path; deps = [] }
  else { name = Filename.basename path; path; deps = [] }
