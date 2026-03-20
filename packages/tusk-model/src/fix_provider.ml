open Std
open Std.Data

type t = {
  name : string;
  package_name : string;
  package_path : Path.t;
  source_path : Path.t;
  rules : string list;
}

let normalize_rule_id package_name rule_id =
  if String.contains rule_id ":" then rule_id
  else package_name ^ ":" ^ rule_id

let default_source_paths =
  [
    Path.v "src/tusk_fix_rules/tusk_fix_rules.ml";
    Path.v "src/tusk_fix_rules.ml";
  ]

let resolve_source_path provider_items ~package_path =
  match List.assoc_opt "path" provider_items with
  | Some (Toml.String source_path) -> Path.v source_path
  | _ ->
      default_source_paths
      |> List.find_opt (fun rel_path ->
             Fs.exists Path.(package_path / rel_path)
             |> Result.unwrap_or ~default:false)
      |> Option.unwrap_or ~default:(Path.v "src/tusk_fix_rules.ml")

let parse_provider provider_toml ~package_name ~package_path =
  match provider_toml with
  | Toml.Table provider_items -> (
      let source_path = resolve_source_path provider_items ~package_path in
      let rules =
        match List.assoc_opt "rules" provider_items with
        | Some (Toml.Array items) ->
            List.filter_map Toml.get_string items
            |> List.map (normalize_rule_id package_name)
        | _ -> []
      in
      Some
        {
          name = package_name;
          package_name;
          package_path;
          source_path = Path.(package_path / source_path);
          rules;
        })
  | _ -> None

let parse_from_toml items ~package_name ~package_path =
  match List.assoc_opt "tusk" items with
  | Some (Toml.Table tusk_items) -> (
      match List.assoc_opt "fix" tusk_items with
      | Some (Toml.Table fix_items) -> (
          match List.assoc_opt "provider" fix_items with
          | Some provider ->
              parse_provider provider ~package_name ~package_path
              |> Option.to_list
          | None -> [])
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
