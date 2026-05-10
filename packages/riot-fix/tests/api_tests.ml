open Std

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_cwd = fun path fn ->
  let original =
    Env.current_dir ()
    |> Result.expect ~msg:"failed to get cwd"
  in
  Env.set_current_dir path
  |> Result.expect ~msg:"failed to chdir into test dir";
  try
    let result = fn () in
    Env.set_current_dir original
    |> Result.expect ~msg:"failed to restore cwd";
    result
  with
  | exn ->
      Env.set_current_dir original
      |> Result.expect ~msg:"failed to restore cwd after exception";
      raise exn

let parse_matches = fun argv ->
  match ArgParser.get_matches Riot_fix.Cli.command ("fix" :: argv) with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches -> Ok matches

let json_field = fun fields name ->
  fields
  |> List.find ~fn:(fun (field_name, _) -> String.equal field_name name)
  |> Option.map ~fn:(fun (_, value) -> value)

let tests = [
  Test.case
    "fix_request_of_matches parses explicit check requests"
    (fun _ctx ->
      with_tempdir
        "riot_fix_api"
        (fun tmpdir ->
          with_cwd
            tmpdir
            (fun () ->
              match parse_matches [ "--json"; "--check"; "sample.ml" ] with
              | Error _ as err -> err
              | Ok matches ->
                  match Riot_fix.fix_request_of_matches matches with
                  | Error err -> Error (Exception.to_string err)
                  | Ok request ->
                      match request.action with
                      | Riot_fix.Run { mode; target; output_mode; _ } ->
                          Test.assert_equal ~expected:Riot_fix.Runner.Check ~actual:mode;
                          Test.assert_equal ~expected:(Path.v "sample.ml") ~actual:target;
                          Test.assert_equal
                            ~expected:(Riot_fix.Report Riot_fix.Reporter.Json)
                            ~actual:output_mode;
                          Ok ()
                      | _ -> Error "expected run request")));
  Test.case
    "fix list-rules returns structured output"
    (fun _ctx ->
      with_tempdir
        "riot_fix_api"
        (fun tmpdir ->
          with_cwd
            tmpdir
            (fun () ->
              match parse_matches [ "--list-rules" ] with
              | Error _ as err -> err
              | Ok matches ->
                  match Riot_fix.fix_request_of_matches matches with
                  | Error err -> Error (Exception.to_string err)
                  | Ok request ->
                      match Riot_fix.fix request with
                      | Error err -> Error (Exception.to_string err)
                      | Ok response ->
                          match Riot_fix.response_output response with
                          | Some output ->
                              Test.assert_true (String.contains output "snake-case-type-names");
                              Ok ()
                          | None -> Error "expected list-rules output")));
  Test.case
    "event to_json encodes progress events"
    (fun _ctx ->
      let event = Riot_fix.Event.FileProgress {
        file = Path.v "sample.ml";
        progress = {
          phase = Fixme.Source_runner.RuleStarted {
            rule_id = Fixme.Rule_id.from_string "riot:snake-case-type-names";
          };
          timestamp_ms = 42;
        };
      }
      in
      match Riot_fix.Event.to_json event with
      | Data.Json.Object fields ->
          Test.assert_equal
            ~expected:(Some (Data.Json.String "progress"))
            ~actual:(json_field fields "type");
          Test.assert_equal
            ~expected:(Some (Data.Json.String "rule_started"))
            ~actual:(json_field fields "stage");
          Ok ()
      | _ -> Error "expected JSON object");
  Test.case
    "fix check emits events through the top-level api"
    (fun _ctx ->
      with_tempdir
        "riot_fix_api"
        (fun tmpdir ->
          let sample = Path.(tmpdir / Path.v "sample.ml") in
          Fs.write "type userProfile = int\n" sample
          |> Result.expect ~msg:"failed to write sample";
          let seen = ref [] in
          let request: Riot_fix.fix_request = {
            cwd = tmpdir;
            scope = None;
            action =
              Riot_fix.Run {
                mode = Riot_fix.Runner.Check;
                limit = None;
                target = sample;
                output_mode = Riot_fix.Silent;
                use_generated_runner = false;
              };
          }
          in
          match Riot_fix.fix ~on_event:(fun event -> seen := event :: !seen) request with
          | Ok _ -> Error "expected issues to remain"
          | Error err ->
              Test.assert_true
                (String.contains (Exception.to_string err) "Issues remain after riot fix");
              Test.assert_true (not (List.is_empty !seen));
              Ok ()));
  Test.case
    "fix_request_of_matches enables generated runner when providers are present"
    (fun _ctx ->
      with_tempdir
        "riot_fix_api"
        (fun tmpdir ->
          with_cwd
            tmpdir
            (fun () ->
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
              let fix_dir = Path.(package_dir / Path.v "fix") in
              Fs.create_dir_all fix_dir
              |> Result.expect ~msg:"failed to create fix dir";
              Fs.write
                {|
[workspace]
members = ["packages/demo"]
|}
                Path.(tmpdir / Path.v "riot.toml")
              |> Result.expect ~msg:"failed to write workspace riot.toml";
              Fs.write
                {|
[package]
name = "demo"
version = "0.1.0"

[riot.fix.provider]
rules = ["demo-rule"]
|}
                Path.(package_dir / Path.v "riot.toml")
              |> Result.expect ~msg:"failed to write package riot.toml";
              Fs.write
                "let rules () = []\nlet explanations () = []\n"
                Path.(fix_dir / Path.v "riot_fix_rules.ml")
              |> Result.expect ~msg:"failed to write provider";
              match parse_matches [ "--check"; "sample.ml" ] with
              | Error _ as err -> err
              | Ok matches ->
                  match Riot_fix.fix_request_of_matches matches with
                  | Error err -> Error (Exception.to_string err)
                  | Ok request ->
                      match request.action with
                      | Riot_fix.Run { use_generated_runner; _ } ->
                          Test.assert_equal ~expected:true ~actual:use_generated_runner;
                          Ok ()
                      | _ -> Error "expected run request")));
]

let main ~args:_ =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-fix:api" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
