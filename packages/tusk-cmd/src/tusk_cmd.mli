open Std
(** Interface that package commands must implement *)
module type Command = sig
  val name: string
  (** Command name (must match TOML declaration) *)
  val command: ArgParser.command
  (** Full ArgParser command with subcommands, args, etc. *)
  val run: args:ArgParser.matches -> (unit, string) result
  (** Execute the command with parsed arguments *)
end
(** Global registry for dynamically loaded commands *)
module Registry: sig
  val register: (module Command) -> unit
  (** Register a command (called by plugin initialization) *)
  val get: string -> (module Command) option
  (** Lookup a registered command by name *)
  val list: unit -> (string * (module Command)) list
  (** List all registered commands *)
end
