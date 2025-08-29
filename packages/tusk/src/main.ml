(** Main entry point for tusk *)

(* Ignore SIGPIPE to prevent exit code 141 when output is piped *)
let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let () = Miniriot.run ~main:Cli.main |> exit
