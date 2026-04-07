open Std

(** Top-level Riot CLI bootstrap and dispatch. *)

(** Build the root command tree for the Riot CLI. *)
val build_cli: unit -> ArgParser.command

(** Initialize runtime process state required by the CLI before command dispatch. *)
val initialize_runtime: unit -> unit

(** Run the Riot CLI for an explicit argument list. *)
val run: args:string list -> (unit, exn) result

(** Main CLI entry point used by the installed binary. *)
val main: args:string list -> (unit, exn) result
