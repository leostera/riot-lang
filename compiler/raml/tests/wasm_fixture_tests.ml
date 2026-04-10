open Std

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"raml:wasm_fixture_tests" ~tests:[] ~args)
    ~args:Env.args
    ()
