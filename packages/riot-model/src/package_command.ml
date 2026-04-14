open Std
open Std.Data

type t = {
  name: string;
  description: string;
  package_name: Package_name.t;
  package_path: Path.t;
  command_module: string;
  command_source: Path.t;
  command_binary: Path.t;
}

let is_built: t -> bool = fun cmd ->
  match Fs.exists cmd.command_binary with
  | Ok true -> true
  | _ -> false

let status_string: t -> string = fun cmd ->
  if is_built cmd then
    "ready"
  else
    "not built"

let parse_from_toml:
  Toml.value list ->
  package_name:Package_name.t ->
  package_path:Path.t ->
  t list =
fun toml_entries ~package_name ~package_path ->
  List.filter_map toml_entries
    ~fn:(fun cmd_toml ->
      match cmd_toml with
      | Toml.Table cmd_items -> (
          match (Fields.get "name" cmd_items, Fields.get "path" cmd_items) with
          | (Some (Toml.String name), Some (Toml.String path)) ->
              let description =
                match Fields.get "help" cmd_items with
                | Some (Toml.String h) -> h
                | _ -> ""
              in
              let command_source = Path.v path in
              (* Extract module name from source file for display *)
              let command_module =
                command_source
                |> Path.basename
                |> (fun s ->
                  match String.index_of s ~char:'.' with
                  | Some idx -> String.sub s ~offset:0 ~len:idx
                  | None -> s)
                |> String.capitalize_ascii
              in
              (* Command binary name is the command name from TOML *)
              let command_binary =
                Path.(
                  v "_build"
                  / v "debug"
                  / v "out"
                  / v (Package_name.to_string package_name)
                  / v name)
              in
              Some {
                name;
                description;
                package_name;
                package_path;
                command_module;
                command_source;
                command_binary;
              }
          | _ -> None
        )
      | _ -> None)

(* Note: discover_all and find_by_name moved to Workspace module to avoid circular dependency *)

let to_json: t -> Json.t = fun cmd ->
  Json.Object [
    ("name", Json.String cmd.name);
    ("description", Json.String cmd.description);
    ("package", Json.String (Package_name.to_string cmd.package_name));
    ("command_binary", Json.String (Path.to_string cmd.command_binary));
    ("is_built", Json.Bool (is_built cmd));
  ]
