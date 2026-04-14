open Std

type t = {
  name: Riot_model.Package_name.t;
  requirement: Std.Version.requirement option;
}

type error =
  | Invalid_spec of { spec: string; error: string }

val from_string: string -> (t, error) result

val to_string: t -> string

val error_message: error -> string
