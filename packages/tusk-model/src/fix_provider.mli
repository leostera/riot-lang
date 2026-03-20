open Std
open Std.Data

type t = {
  name : string;
  package_name : string;
  package_path : Path.t;
  source_path : Path.t;
  rules : string list;
}

val parse_from_toml :
  (string * Toml.value) list ->
  package_name:string ->
  package_path:Path.t ->
  t list

val to_json : t -> Json.t
