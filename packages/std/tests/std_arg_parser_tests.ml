open Std

module Test = Std.Test

let command_with_optional_subcommand = fun () ->
  let open ArgParser in
  let open ArgParser.Arg in
  command "tool"
  |> args
    [
      flag "list"
      |> long "list";
    ]
  |> allow_no_subcommand
  |> subcommands
    [
      command "clean"
      |> args
        [
          flag "force"
          |> long "force";
        ];
    ]

let run_like_command = fun () ->
  let open ArgParser in
  let open ArgParser.Arg in
  command "run"
  |> allow_trailing_args
  |> args
    [
      positional "name"
      |> required false;
      option "package"
      |> short 'p'
      |> long "package";
      flag "watch"
      |> short 'w'
      |> long "watch";
    ]

let test_command_allows_flags_without_subcommand = fun _ctx ->
  match ArgParser.get_matches (command_with_optional_subcommand ()) [ "tool"; "--list" ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches ->
      if not (ArgParser.get_flag matches "list") then
        Error "expected --list to be set"
      else if Option.is_some (ArgParser.get_subcommand matches) then
        Error "expected no subcommand"
      else
        Ok ()

let test_command_still_parses_subcommand = fun _ctx ->
  match ArgParser.get_matches (command_with_optional_subcommand ()) [ "tool"; "clean"; "--force" ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | Some ("clean", sub_matches) ->
          if ArgParser.get_flag sub_matches "force" then
            Ok ()
          else
            Error "expected subcommand --force to be set"
      | Some (name, _) -> Error ("unexpected subcommand: " ^ name)
      | None -> Error "expected clean subcommand"

let test_trailing_separator_stops_parsing_after_positionals = fun _ctx ->
  match ArgParser.get_matches (run_like_command ()) [ "run"; "-w"; "riot"; "--"; "build" ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches ->
      if not (ArgParser.get_flag matches "watch") then
        Error "expected -w before positional to parse as run flag"
      else (
        Test.assert_equal ~expected:(Some "riot") ~actual:(ArgParser.get_one matches "name");
        Test.assert_equal ~expected:[ "build" ] ~actual:(ArgParser.trailing_args matches);
        Ok ()
      )

let test_flags_after_positionals_still_parse_before_separator = fun _ctx ->
  match ArgParser.get_matches (run_like_command ()) [ "run"; "riot"; "-w"; "--"; "build" ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches ->
      if not (ArgParser.get_flag matches "watch") then
        Error "expected -w after positional to parse before --"
      else (
        Test.assert_equal ~expected:(Some "riot") ~actual:(ArgParser.get_one matches "name");
        Test.assert_equal ~expected:[ "build" ] ~actual:(ArgParser.trailing_args matches);
        Ok ()
      )

let test_options_after_flags_parse_before_separator = fun _ctx ->
  match ArgParser.get_matches (run_like_command ()) [ "run"; "-w"; "-p"; "riot-cli"; "riot" ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches ->
      if not (ArgParser.get_flag matches "watch") then
        Error "expected -w to parse"
      else (
        Test.assert_equal ~expected:(Some "riot-cli") ~actual:(ArgParser.get_one matches "package");
        Test.assert_equal ~expected:(Some "riot") ~actual:(ArgParser.get_one matches "name");
        Test.assert_equal ~expected:[] ~actual:(ArgParser.trailing_args matches);
        Ok ()
      )

let test_unknown_flags_before_separator_are_errors = fun _ctx ->
  match ArgParser.get_matches (run_like_command ()) [ "run"; "riot"; "-z"; "--"; "build" ] with
  | Error (ArgParser.UnknownArgument "-z") -> Ok ()
  | Error err -> Error ("expected unknown -z error, got: " ^ ArgParser.error_message err)
  | Ok _ -> Error "expected unknown -z before -- to fail"

let test_unknown_flags_after_separator_are_trailing = fun _ctx ->
  match ArgParser.get_matches
    (run_like_command ())
    [ "run"; "riot"; "--"; "-z"; "--package"; "child"; ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "riot") ~actual:(ArgParser.get_one matches "name");
      Test.assert_equal
        ~expected:[ "-z"; "--package"; "child" ]
        ~actual:(ArgParser.trailing_args matches);
      Ok ()

let tests = [
  Test.case
    "arg parser allows flags without optional subcommand"
    test_command_allows_flags_without_subcommand;
  Test.case "arg parser still parses optional subcommands" test_command_still_parses_subcommand;
  Test.case
    "arg parser captures trailing args only after separator"
    test_trailing_separator_stops_parsing_after_positionals;
  Test.case
    "arg parser parses flags after positionals before separator"
    test_flags_after_positionals_still_parse_before_separator;
  Test.case
    "arg parser parses options after flags before separator"
    test_options_after_flags_parse_before_separator;
  Test.case
    "arg parser rejects unknown flags before separator"
    test_unknown_flags_before_separator_are_errors;
  Test.case
    "arg parser captures unknown flags after separator"
    test_unknown_flags_after_separator_are_trailing;
]

let main ~args = Test.Cli.main ~name:"std_arg_parser_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
