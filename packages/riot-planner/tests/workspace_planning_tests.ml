open Std

module Test = Std.Test

let tests = []

let main ~args = Test.Cli.main ~name:"workspace_planning_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
