open Std

(** Repository-local operational config loaded from [.riot/config.toml]. *)

type cache_policy = {
  keep_generations: int;
  max_size_bytes: int64;
}

type t = {
  cache: cache_policy;
}

type error =
  | ReadFailed of { path: Path.t; error: string }
  | ParseFailed of { path: Path.t; error: string }
  | InvalidConfig of { path: Path.t; error: string }

val default_cache_policy: cache_policy

val default: t

val message: error -> string

val load: workspace_root:Path.t -> (t, error) result
