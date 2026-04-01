open Std

let () =
  Miniriot.run ~main:(fun ~args -> Tusk_fix.Cli.main ~args ()) ~args:Env.args ()
