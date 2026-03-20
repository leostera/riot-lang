open Std
open Std.Data

type t = {
  name : string;
  package_name : string;
  package_path : Path.t;
  source_path : Path.t;
  rules : string list;
}

let normalize_rule_id rule_id =
  if String.starts_with ~prefix:"pkg:" rule_id then rule_id
  else "pkg:" ^ rule_id

let parse_provider provider_toml ~package_name ~package_path =
  match provider_toml with
  | Toml.Table provider_items -> (
      match
        ( List.assoc_opt "name" provider_items,
          List.assoc_opt "path" provider_items )
      with
      | Some (Toml.String name), Some (Toml.String source_path) ->
          let rules =
            match List.assoc_opt "rules" provider_items with
            | Some (Toml.Array items) ->
                List.filter_map Toml.get_string items
                |> List.map normalize_rule_id
            | _ -> []
          in
          Some
            {
              name;
              package_name;
              package_path;
              source_path = Path.(package_path / Path.v source_path);
              rules;
            }
      | _ -> None)
  | _ -> None

let parse_from_toml items ~package_name ~package_path =
  match List.assoc_opt "tusk" items with
  | Some (Toml.Table tusk_items) -> (
      match List.assoc_opt "fix" tusk_items with
      | Some (Toml.Table fix_items) -> (
          match List.assoc_opt "provider" fix_items with
          | Some (Toml.Array providers) ->
              List.filter_map
                (fun provider ->
                  parse_provider provider ~package_name ~package_path)
                providers
          | _ -> [])
      | _ -> [])
  | _ -> []

let to_json provider =
  Json.Object
    [
      ("name", Json.String provider.name);
      ("package_name", Json.String provider.package_name);
      ("package_path", Json.String (Path.to_string provider.package_path));
      ("source_path", Json.String (Path.to_string provider.source_path));
      ("rules", Json.Array (List.map (fun rule -> Json.String rule) provider.rules));
    ]
