(** Main entry point for tusk *)

let main args =
  (* Ignore SIGPIPE to prevent exit code 141 when output is piped *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Miniriot.run ~main:Cli.main ~args |> exit
;;

main Std.Env.args
