open Std

module Test = Std.Test

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 1_024
  |> with_dictionary [ ""; "0.0.0"; "1.2.3"; ">= 1.0.0"; "~> 1.2"; "*"; ])

let test_pubgrub_fuzz = fun _ctx input ->
  Pubgrub.version_of_string input
  |> ignore;
  Version.parse_requirement input
  |> ignore;
  (
    match Pubgrub.version_of_string input with
    | Error _ -> ()
    | Ok version ->
        Pubgrub.singleton version
        |> ignore;
        Pubgrub.higher_than version
        |> ignore;
        Pubgrub.lower_than version
        |> ignore
  );
  Ok ()

let tests =
  Test.[
    fuzz
      "pubgrub version and range parsers accept arbitrary text"
      ~seeds:[ ""; "0.0.0"; "1.2.3-alpha"; ">= 1.0.0"; ]
      ~mutator
      test_pubgrub_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"pubgrub_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
