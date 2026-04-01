open Std

type registry = {
  api_token: string option;
}
type t = {
  registries: (string * registry) list;
}
type error =
  | ReadFailed of { path: Path.t; error: string }
  | ParseFailed of { path: Path.t; error: string }
  | InvalidConfig of { error: string }
  | InvalidRegistryConfig of { registry_name: string; error: string }
val empty: t

val message: error -> string

val of_toml: Std.Data.Toml.value -> (t, error) result

val load: Path.t -> (t, error) result

val api_token: t -> registry_name:string -> string option
