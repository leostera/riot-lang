(** Main entry point for tusk *)

let main args =
  Std.Log.(set_level Info);

  (* Ignore SIGPIPE to prevent exit code 141 when output is piped *)
  Kernel.System.(set_signal sigpipe Signal_ignore);

  Miniriot.run ~main:Tusk.Cli.main ~args;
  0
;;

main Std.Env.args
