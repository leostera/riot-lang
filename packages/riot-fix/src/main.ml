open Std

let main ~args = Riot_fix.Cli.main ~args ()

let () = Runtime.run ~main ~args:Env.args ()
