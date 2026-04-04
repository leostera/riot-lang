open Std
module Test = Std.Test

let parse_check = fun args ->
  match ArgParser.get_matches Riot_cli.Check_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_check_accepts_json_flag = fun _ctx ->
  match parse_check [ "check"; "--json"; "app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let test_check_accepts_quiet_flag = fun _ctx ->
  match parse_check [ "check"; "--quiet"; "app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "quiet" then
        Ok ()
      else
        Error "expected --quiet flag to be parsed"

let test_check_accepts_explain_option = fun _ctx ->
  match parse_check [ "check"; "--explain"; "TYP2001" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "TYP2001") ~actual:(ArgParser.get_one matches "explain");
      Ok ()

let test_check_accepts_path_argument = fun _ctx ->
  match parse_check [ "check"; "packages/app/src/app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      let expected = Path.v "packages/app/src/app.ml" in
      Test.assert_equal ~expected:(Some expected) ~actual:(ArgParser.get_path matches "path");
      Ok ()

let tests =
  Test.[
    case "check: parse --json flag" test_check_accepts_json_flag;
    case "check: parse --quiet flag" test_check_accepts_quiet_flag;
    case "check: parse --explain option" test_check_accepts_explain_option;
    case "check: parse path argument" test_check_accepts_path_argument;
  ]

let name = "Riot CLI Check Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
