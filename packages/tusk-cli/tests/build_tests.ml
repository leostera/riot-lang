open Std

module Test = Std.Test

let parse_build args =
  match ArgParser.get_matches Tusk_cli.Build.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_build_accepts_multiple_packages () =
  match parse_build [ "build"; "syn"; "krasny"; "tusk-cli" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      let actual = ArgParser.get_many matches "package" in
      Test.assert_equal ~expected:[ "syn"; "krasny"; "tusk-cli" ] ~actual;
      Ok ()

let test_build_usage_shows_variadic_packages () =
  let usage = ArgParser.usage_string Tusk_cli.Build.command in
  if String.contains usage "package..." then
    Ok ()
  else
    Error ("expected variadic package usage, got: " ^ usage)

let tests =
  Test.
    [
      case "build: accept multiple package arguments"
        test_build_accepts_multiple_packages;
      case "build: usage shows variadic packages"
        test_build_usage_shows_variadic_packages;
    ]

let name = "Tusk CLI Build Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
