open Std
module Test = Std.Test

let parse_login = fun args ->
  match ArgParser.get_matches Riot_cli.Login.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_logout = fun args ->
  match ArgParser.get_matches Riot_cli.Logout.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_yank = fun args ->
  match ArgParser.get_matches Riot_cli.Yank.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_login_accepts_token_option = fun _ctx ->
  match parse_login [ "login"; "--token"; "root-secret" ] with
  | Error err -> Error ("expected login args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "root-secret") ~actual:(ArgParser.get_one matches "token");
      Ok ()

let test_logout_accepts_no_args = fun _ctx ->
  match parse_logout [ "logout" ] with
  | Error err -> Error ("expected logout args to parse: " ^ err)
  | Ok _ -> Ok ()

let test_yank_accepts_package_version_spec = fun _ctx ->
  match parse_yank [ "yank"; "std@0.1.0" ] with
  | Error err -> Error ("expected yank args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "std@0.1.0") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_yank_request_rejects_missing_version = fun _ctx ->
  match
    Riot_cli.Yank.parse_request
      (parse_yank [ "yank"; "std" ] |> Result.expect ~msg:"expected args to parse")
  with
  | Error (Riot_cli.Yank.InvalidPackageSpec _) -> Ok ()
  | Error err -> Error ("expected invalid package spec error, got: " ^ Riot_cli.Yank.message err)
  | Ok _ -> Error "expected yank request parsing to reject missing version"

let tests =
  Test.[
    case "auth: login parses --token" test_login_accepts_token_option;
    case "auth: logout parses without args" test_logout_accepts_no_args;
    case "auth: yank parses package@version" test_yank_accepts_package_version_spec;
    case "auth: yank rejects missing version" test_yank_request_rejects_missing_version;
  ]

let name = "Riot CLI Auth Tests"

let () = Actors.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
