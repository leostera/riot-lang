(** Main entry point for tusk *)

let main args =
  (* Ignore SIGPIPE to prevent exit code 141 when output is piped *)
  Kernel.System.set_signal Kernel.System.sigpipe Kernel.System.Signal_ignore;
  Miniriot.run ~main:Cli.main ~args |> exit
;;

main Std.Env.args
