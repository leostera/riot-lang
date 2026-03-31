open Global

type env =
  | Dev
  | Test
  | Prod
val detect_env: unit -> env

val env_to_string: env -> string

val config_path: env -> string

val load_file: string -> (Data.Toml.value, string) result

val load_for_env: env -> (Data.Toml.value, string) result

val extract_app_section: string -> Data.Toml.value -> (Data.Toml.value, string) result
