open Std
module Test = Std.Test

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let write_file = fun path content ->
  match Fs.create_dir_all (Path.dirname path) with
  | Error err -> Error (IO.error_message err)
  | Ok () -> (
      match Fs.write content path with
      | Ok () -> Ok ()
      | Error err -> Error (IO.error_message err)
    )

let parse_upgrade = fun args ->
  match ArgParser.get_matches Riot_cli.Upgrade.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_upgrade_accepts_version_flag = fun _ctx ->
  match parse_upgrade [ "upgrade"; "--version"; "abc123" ] with
  | Error err -> Error ("expected upgrade args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "abc123") ~actual:(ArgParser.get_one matches "version");
      Ok ()

let test_run_install_script_sets_version_env = fun _ctx ->
  with_tempdir_result "upgrade-script"
    (fun tempdir ->
      let script_path = Path.(tempdir / Path.v "install.sh") in
      let output_path = Path.(tempdir / Path.v "version.txt") in
      let script = String.concat "\n" [
        "printf '%s' \"${RIOT_VERSION:-latest}\" > \"$RIOT_UPGRADE_TEST_OUTPUT\"";
        "";
      ] in
      match write_file script_path script with
      | Error _ as err -> err
      | Ok () -> (
          match Riot_cli.Upgrade.run_install_script
            ~env:[ ("RIOT_UPGRADE_TEST_OUTPUT", Path.to_string output_path) ]
            ~version:"deadbeef"
            ~script_path
            () with
          | Error message -> Error message
          | Ok () -> (
              match Fs.read output_path with
              | Error err -> Error (IO.error_message err)
              | Ok actual ->
                  Test.assert_equal ~expected:"deadbeef" ~actual;
                  Ok ()
            )
        ))

let test_run_install_script_reports_failure = fun _ctx ->
  with_tempdir_result "upgrade-script-fail"
    (fun tempdir ->
      let script_path = Path.(tempdir / Path.v "install.sh") in
      match write_file script_path "exit 17\n" with
      | Error _ as err -> err
      | Ok () -> (
          match Riot_cli.Upgrade.run_install_script ~script_path () with
          | Ok () -> Error "expected failing install script to return an error"
          | Error message ->
              if String.contains message "status 17" then
                Ok ()
              else
                Error ("expected failure message to mention status 17, got: " ^ message)
        ))

let tests =
  Test.[
    case "upgrade: parse --version flag" test_upgrade_accepts_version_flag;
    case "upgrade: install script receives version env" test_run_install_script_sets_version_env;
    case "upgrade: install script reports non-zero exit" test_run_install_script_reports_failure;
  ]

let name = "Riot CLI Upgrade Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
