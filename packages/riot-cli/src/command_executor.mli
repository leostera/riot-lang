open Std

(** Execute a compiled Riot command as a subprocess.

    Use this helper when the CLI needs to hand off control to another command
    binary while preserving the caller's argument list.
*)
val execute:
  (** Path to the compiled command binary to run. *)
  command_binary:Path.t ->
  (** Command-line arguments forwarded to the subprocess. *)
  args:string list ->
  (unit, exn) result
