open Std

val execute: command_binary:Path.t -> args:string list -> (unit, exn) result
(** Execute a command binary by delegating to it as a subprocess.
    
    @param command_binary Path to the compiled command binary
    @param args Command-line arguments to pass to the command
    @return Ok () on success, Error on failure
*)
