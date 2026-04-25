open Std
module Test = Std.Test

let tests = []

let main ~args = Test.Cli.main ~name:"blink_integration" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
