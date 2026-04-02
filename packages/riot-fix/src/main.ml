open Std

let () =
  Actors.run ~main:(fun ~args -> Riot_fix.Cli.main ~args ()) ~args:Env.args ()
