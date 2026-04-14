open Std

type t =
  | Host
  | All
  | Pattern of string

type error = {
  pattern: string;
  available_targets: string list;
}

type context

val configured_targets:
  host:string -> Riot_model.Toolchain_config.t -> string list

val create: host:string -> configured_targets:string list -> context

val of_cli_options: all_targets:bool -> target:string option -> t

val of_string: string -> t

val resolve: context -> t -> (string list, error) result
