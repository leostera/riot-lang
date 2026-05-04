open Std

type t = {
  ignore_patterns: string list;
}

val empty: t

val from_toml: Std.Data.Toml.value -> t

val load: Path.t -> t
