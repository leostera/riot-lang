open Std
module Test = Std.Test

let tests = []

let () = Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"blink_integration" ~tests ~args) ~args:Env.args ()
