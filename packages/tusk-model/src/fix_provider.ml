open Std
open Std.Data

type t = {
  name : string;
  package_name : string;
  package_path : Path.t;
  module_name : string;
  rules : string list;
}

let parse_provider provider_toml ~package_name ~package_path =
  match provider_toml with
  | Toml.Table provider_items -> (
      match
        ( List.assoc_opt "name" provider_items,
          List.assoc_opt "module" provider_items )
      with
      | Some (Toml.String name), Some (Toml.String module_name) ->
          let rules =
            match List.assoc_opt "rules" provider_items with
            | Some (Toml.Array items) -> List.filter_map Toml.get_string items
            | _ -> []
          in
          Some
            {
              name;
              package_name;
              package_path;
              module_name;
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
      ("module_name", Json.String provider.module_name);
      ("rules", Json.Array (List.map (fun rule -> Json.String rule) provider.rules));
    ]
