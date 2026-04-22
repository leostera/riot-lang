open Std
module Test = Std.Test

let parse_search = fun args ->
  match ArgParser.get_matches Riot_cli.Search.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_search_accepts_limit_option = fun _ctx ->
  match parse_search [ "search"; "mini"; "--limit"; "7" ] with
  | Error err -> Error ("expected search args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "7") ~actual:(ArgParser.get_one matches "limit");
      Ok ()

let test_search_accepts_json_flag = fun _ctx ->
  match parse_search [ "search"; "mini"; "--json" ] with
  | Error err -> Error ("expected search args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let tests =
  Test.[
    case "search: parse --limit option" test_search_accepts_limit_option;
    case "search: parse --json flag" test_search_accepts_json_flag;
  ]

let name = "Riot CLI Search Tests"

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
