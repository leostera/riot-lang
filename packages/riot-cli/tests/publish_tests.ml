open Std

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let parse_publish = fun args ->
  match ArgParser.get_matches Riot_cli.Publish.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_publish_accepts_package_option = fun _ctx ->
  match parse_publish [ "publish"; "-p"; "demo" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "demo") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_publish_accepts_workspace_flag = fun _ctx ->
  match parse_publish [ "publish"; "--workspace" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "workspace" then
        Ok ()
      else
        Error "expected --workspace flag to be parsed"

let test_publish_accepts_dry_run_flag = fun _ctx ->
  match parse_publish [ "publish"; "--dry-run" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "dry-run" then
        Ok ()
      else
        Error "expected --dry-run flag to be parsed"

let test_publish_accepts_skip_check_flag = fun _ctx ->
  match parse_publish [ "publish"; "--skip-check" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "skip-check" then
        Ok ()
      else
        Error "expected --skip-check flag to be parsed"

let test_publish_accepts_json_flag = fun _ctx ->
  match parse_publish [ "publish"; "--json" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let test_publish_conflicting_selection_fails = fun _ctx ->
  match Riot_cli.Publish.resolve_request
    ~package_name:(Some (package_name "demo"))
    ~workspace_mode:true with
  | Error Riot_cli.Publish.ConflictingSelection -> Ok ()
  | Ok _ -> Error "expected conflicting publish selection to fail"
  | Error err -> Error ("unexpected publish selection error: " ^ Riot_cli.Publish.message err)

let tests =
  Test.[
    case "publish: parse -p option" test_publish_accepts_package_option;
    case "publish: parse --workspace flag" test_publish_accepts_workspace_flag;
    case "publish: parse --dry-run flag" test_publish_accepts_dry_run_flag;
    case "publish: parse --skip-check flag" test_publish_accepts_skip_check_flag;
    case "publish: parse --json flag" test_publish_accepts_json_flag;
    case "publish: conflicting selection fails" test_publish_conflicting_selection_fails;
  ]

let name = "Riot CLI Publish Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
