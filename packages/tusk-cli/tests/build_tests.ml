open Std

module Test = Std.Test

let parse_build args =
  match ArgParser.get_matches Tusk_cli.Build.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let contains_substring haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop i =
    if i > haystack_len - needle_len then
      false
    else if String.sub haystack i needle_len = needle then
      true
    else
      loop (i + 1)
  in
  needle_len = 0 || loop 0

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

let test_build_accepts_json_flag () =
  match parse_build [ "build"; "--json"; "syn" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then Ok ()
      else Error "expected --json flag to be parsed"

let test_build_usage_shows_json_flag () =
  let usage = ArgParser.usage_string Tusk_cli.Build.command in
  if contains_substring usage "--json" then
    Ok ()
  else
    Error ("expected json flag in usage, got: " ^ usage)

let tests =
  Test.
    [
      case "build: accept multiple package arguments"
        test_build_accepts_multiple_packages;
      case "build: usage shows variadic packages"
        test_build_usage_shows_variadic_packages;
      case "build: parse --json flag" test_build_accepts_json_flag;
      case "build: usage mentions json output mode" test_build_usage_shows_json_flag;
    ]

let name = "Tusk CLI Build Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
