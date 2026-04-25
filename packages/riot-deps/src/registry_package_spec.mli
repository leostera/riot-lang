open Std

type t = { name: Riot_model.Package_name.t; requirement: Std.Version.requirement option }

type error =
  | InvalidShape of { spec: string }
  | InvalidPackageName of { spec: string; name: string; error: Riot_model.Package_name.error }
  | InvalidRequirement of { spec: string; requirement: string; error: Std.Version.parse_error }

val from_string: string -> (t, error) result

val to_string: t -> string

val error_message: error -> string
