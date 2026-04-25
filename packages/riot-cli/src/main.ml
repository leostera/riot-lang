open Std

let main ~args =
  Log.(set_level Info);
  Riot_cli.Cli.main ~args

let () = Runtime.run ~main ~args:Env.args ()
