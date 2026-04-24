open Std

type error =
  | StatFailed of { path: Path.t; error: IO.error }
  | ReadFailed of { path: Path.t; error: IO.error }
  | TomlParseFailed of { path: Path.t; error: Std.Data.Toml.error }
  | DecodeFailed of { path: Path.t; error: Riot_model.Lockfile.error }
  | WriteFailed of { path: Path.t; error: IO.error }
val error_message: error -> string

val read: workspace_root:Path.t -> (Riot_model.Lockfile.t option, error) result

val write: workspace_root:Path.t -> Riot_model.Lockfile.t -> (unit, error) result
