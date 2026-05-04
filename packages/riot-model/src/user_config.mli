open Std

type registry = {
  api_url: Net.Uri.t;
  cdn_url: Net.Uri.t;
  api_token: string option;
}
type t = {
  registries: (string * registry) list;
}
type registry_field =
  | Api_url
  | Cdn_url
  | Api_token
type config_error =
  | RegistryMustBeTable
type registry_error =
  | InvalidDefaultUri of {
      field: registry_field;
      error: Net.Uri.error;
    }
  | InvalidUri of {
      field: registry_field;
      error: Net.Uri.error;
    }
  | FieldMustBeString of registry_field
  | RegistryEntryMustBeTable
type error =
  | ReadFailed of {
      path: Path.t;
      error: IO.error;
    }
  | ParseFailed of {
      path: Path.t;
      error: Std.Data.Toml.error;
    }
  | WriteFailed of {
      path: Path.t;
      error: IO.error;
    }
  | InvalidConfig of config_error
  | InvalidRegistryConfig of {
      registry_name: string;
      error: registry_error;
    }

val empty: t

val default: t

val message: error -> string

val from_toml: Std.Data.Toml.value -> (t, error) result

val load: Path.t -> (t, error) result

val save: t -> Path.t -> (unit, error) result

val api_token: t -> registry_name:string -> string option

val set_api_token: t -> registry_name:string -> string -> t

val clear_api_token: t -> registry_name:string -> t
