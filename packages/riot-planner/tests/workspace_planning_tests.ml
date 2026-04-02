open Std
module Test = Std.Test

let tests = []

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"workspace_planning_tests" ~tests ~args)
    ~args:Env.args
    ()
