type t = {
  members: string list;
  ocaml_version: string;
}

let parse_workspace_toml filename =
  let toml = Toml.parse_file filename in
  let members = match Toml.find_value "workspace.members" toml with
    | Some (Toml.Array items) -> 
        List.filter_map Toml.get_string items
    | _ -> [] in
  let ocaml_version = match Toml.find_value "workspace.config.ocaml_version" toml with
    | Some s -> (match Toml.get_string s with Some v -> v | None -> "5.3.0")
    | None -> "5.3.0" in
  { members; ocaml_version }