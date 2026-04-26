open Std
open Std.Data

type t = {
  name: string;
  package_name: Package_name.t;
  package_path: Path.t;
  source_path: Path.t;
  rules: string list;
}

let normalize_rule_id = fun package_name rule_id ->
  let package_name = Package_name.to_string package_name in
  if String.contains rule_id ":" then
    rule_id
  else
    package_name ^ ":" ^ rule_id

let default_source_paths = [
  Path.v "fix/riot_fix_rules/riot_fix_rules.ml";
  Path.v "fix/riot_fix_rules.ml";
  Path.v "src/riot_fix_rules/riot_fix_rules.ml";
  Path.v "src/riot_fix_rules.ml";
]

let resolve_source_path = fun provider_items ~package_path ->
  match Fields.get "path" provider_items with
  | Some (Toml.String source_path) -> Path.v source_path
  | _ ->
      default_source_paths
      |> List.find
        ~fn:(fun rel_path ->
          Fs.exists Path.(package_path / rel_path)
          |> Result.unwrap_or ~default:false)
      |> Option.unwrap_or ~default:(Path.v "fix/riot_fix_rules.ml")

let parse_provider = fun provider_toml ~package_name ~package_path ->
  match provider_toml with
  | Toml.Table provider_items -> (
      let source_path = resolve_source_path provider_items ~package_path in
      let rules =
        match Fields.get "rules" provider_items with
        | Some (Toml.Array items) ->
            List.filter_map items ~fn:Toml.get_string
            |> List.map ~fn:(normalize_rule_id package_name)
        | _ -> []
      in
      Some {
        name = Package_name.to_string package_name;
        package_name;
        package_path;
        source_path = Path.(package_path / source_path);
        rules;
      }
    )
  | _ -> None

let parse_from_toml = fun items ~package_name ~package_path ->
  match Fields.get "riot" items with
  | Some (Toml.Table riot_items) -> (
      match Fields.get "fix" riot_items with
      | Some (Toml.Table fix_items) -> (
          match Fields.get "provider" fix_items with
          | Some provider ->
              parse_provider provider ~package_name ~package_path
              |> Option.to_list
          | None -> []
        )
      | _ -> []
    )
  | _ -> []

let to_json = fun provider ->
  Json.Object [
    ("name", Json.String provider.name);
    ("package_name", Json.String (Package_name.to_string provider.package_name));
    ("package_path", Json.String (Path.to_string provider.package_path));
    ("source_path", Json.String (Path.to_string provider.source_path));
    ("rules", Json.Array (List.map provider.rules ~fn:(fun rule -> Json.String rule)));
  ]
