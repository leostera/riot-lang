open Std

let () = Actors.run ~main:Raml_cli.Cli.main ~args:Env.args ()
