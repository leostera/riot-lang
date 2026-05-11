open Std

module Test = Std.Test
module Git = Riot_deps.Git_dependency
module RegistrySpec = Riot_deps.Registry_package_spec

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 2_048
  |> with_dictionary
    [
      "";
      "std";
      "std@>=0.0.1";
      "github.com/leostera/riot";
      "leostera/riot";
      "https://github.com/leostera/riot.git#main";
      "github.com/leostera/riot/packages/std#main";
    ])

let test_riot_deps_fuzz = fun _ctx input ->
  Git.looks_like_remote_spec input
  |> ignore;
  Git.parse_spec input
  |> ignore;
  Git.parse_source_locator input
  |> ignore;
  RegistrySpec.from_string input
  |> ignore;
  Ok ()

let tests =
  Test.[
    fuzz
      "riot-deps dependency spec parsers accept arbitrary text"
      ~seeds:[ ""; "std"; "std@>=0.0.1"; "github.com/leostera/riot#main"; ]
      ~mutator
      test_riot_deps_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"riot_deps_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
