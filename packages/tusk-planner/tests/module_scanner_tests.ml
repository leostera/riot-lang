open Std

module Test = Std.Test

let tests = []

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"module_scanner_tests" ~tests ~args)
    ~args:Env.args ()
