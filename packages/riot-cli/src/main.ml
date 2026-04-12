open Std

(** Main entry point for riot *)
let () =
  Std.Log.(set_level Info);
  Runtime.run ~main:Riot_cli.Cli.main ~args:Env.args ()
