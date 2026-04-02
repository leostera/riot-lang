open Std

type registry = {
  api_url: Net.Uri.t;
  cdn_url: Net.Uri.t;
  api_token: string option;
}
type t = {
  registries: (string * registry) list;
}
type error =
  | ReadFailed of { path: Path.t; error: string }
  | ParseFailed of { path: Path.t; error: string }
  | WriteFailed of { path: Path.t; error: string }
  | InvalidConfig of { error: string }
  | InvalidRegistryConfig of { registry_name: string; error: string }
val empty: t

val default: t

val message: error -> string

val of_toml: Std.Data.Toml.value -> (t, error) result

val load: Path.t -> (t, error) result

val save: t -> Path.t -> (unit, error) result

val api_token: t -> registry_name:string -> string option

val set_api_token: t -> registry_name:string -> string -> t

val clear_api_token: t -> registry_name:string -> t
