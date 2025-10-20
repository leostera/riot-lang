type binary = { name : string; path : string }

type t = {
  name : string;
  path : string;
  deps : string list;
  binaries : binary list;
}

let binaries t = t.binaries

let read path =
  let toml_path = Filename.concat path "tusk.toml" in
  if Sys.file_exists toml_path then (
    (* Read the file content *)
    let ic = open_in toml_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;

    (* Parse TOML *)
    match Toml.parse content with
    | Error err ->
        Printf.printf "Error parsing %s: %s\n" toml_path
          (Toml.error_to_string err);
        { name = Filename.basename path; path; deps = []; binaries = [] }
    | Ok (Toml.Table items) ->
        (* Get package name *)
        let name =
          match Toml.find "package" items with
          | Some (Toml.Table pkg_items) -> (
              match Toml.find "name" pkg_items with
              | Some (Toml.String n) -> n
              | _ -> Filename.basename path)
          | _ -> Filename.basename path
        in

        (* Get binaries from [[bin]] array *)
        let binaries =
          match Toml.find "bin" items with
          | Some (Toml.Array bin_tables) ->
              List.filter_map
                (fun bin_value ->
                  match bin_value with
                  | Toml.Table bin_items -> (
                      let bin_name =
                        match Toml.find "name" bin_items with
                        | Some (Toml.String n) -> Some n
                        | _ -> None
                      in
                      let bin_path =
                        match Toml.find "path" bin_items with
                        | Some (Toml.String p) -> Some p
                        | _ -> None
                      in
                      match (bin_name, bin_path) with
                      | Some n, Some p ->
                          Some { name = n; path = Filename.concat path p }
                      | _ -> None)
                  | _ -> None)
                bin_tables
          | _ -> []
        in

        { name; path; deps = []; binaries }
    | _ -> { name = Filename.basename path; path; deps = []; binaries = [] })
  else { name = Filename.basename path; path; deps = []; binaries = [] }
